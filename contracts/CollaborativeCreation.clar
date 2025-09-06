;; Collaborative Content Creation & Attribution System
;; Enables multiple creators to work together on content with transparent attribution and revenue sharing

(define-constant ERR-NOT-AUTHORIZED (err u600))
(define-constant ERR-INVALID-CONTRIBUTION (err u601))
(define-constant ERR-PROJECT-NOT-FOUND (err u602))
(define-constant ERR-ALREADY-MEMBER (err u603))
(define-constant ERR-INVALID-PERCENTAGE (err u604))
(define-constant ERR-INSUFFICIENT-PERMISSIONS (err u605))
(define-constant ERR-PROJECT-FINALIZED (err u606))
(define-constant ERR-INVALID-VOTE (err u607))
(define-constant ERR-VOTING-CLOSED (err u608))

;; Collaborative projects created by teams
(define-map collaborative-projects
    uint
    {
        title: (string-ascii 100),
        description: (string-utf8 500),
        creator: principal,
        status: uint,
        created-at: uint,
        finalized-at: (optional uint),
        total-contributors: uint,
        content-hash: (optional (buff 32)),
        license-type: uint
    }
)

;; Individual contributions to projects
(define-map project-contributions
    { project-id: uint, contributor: principal }
    {
        contribution-type: (string-ascii 30),
        description: (string-utf8 200),
        content-hash: (buff 32),
        timestamp: uint,
        attribution-weight: uint,
        approved: bool,
        votes-for: uint,
        votes-against: uint
    }
)

;; Attribution and revenue sharing for contributors
(define-map contributor-attribution
    { project-id: uint, contributor: principal }
    {
        contribution-percentage: uint,
        role: (string-ascii 50),
        joined-at: uint,
        is-active: bool,
        earned-credits: uint,
        total-contributions: uint
    }
)

;; Voting system for contribution approval
(define-map contribution-votes
    { project-id: uint, contribution-id: uint, voter: principal }
    {
        vote: bool,
        voting-power: uint,
        voted-at: uint
    }
)

;; Project milestones and deadlines
(define-map project-milestones
    { project-id: uint, milestone-id: uint }
    {
        title: (string-ascii 100),
        description: (string-utf8 300),
        deadline: uint,
        status: uint,
        assigned-to: (optional principal),
        completion-date: (optional uint)
    }
)

;; Communication and update logs
(define-map project-updates
    { project-id: uint, update-id: uint }
    {
        author: principal,
        message: (string-utf8 400),
        timestamp: uint,
        update-type: (string-ascii 20)
    }
)

;; Revenue distribution tracking
(define-map revenue-pools
    uint
    {
        total-revenue: uint,
        distributed-amount: uint,
        payment-count: uint,
        last-distribution: uint
    }
)

(define-data-var next-project-id uint u1)
(define-data-var next-update-id uint u1)

;; Project status constants
(define-constant PROJECT_STATUS_ACTIVE u1)
(define-constant PROJECT_STATUS_REVIEW u2)
(define-constant PROJECT_STATUS_FINALIZED u3)
(define-constant PROJECT_STATUS_PUBLISHED u4)

;; Create a new collaborative project
(define-public (create-collaborative-project 
    (title (string-ascii 100)) 
    (description (string-utf8 500)) 
    (license-type uint))
    (let (
        (project-id (var-get next-project-id))
    )
        (map-set collaborative-projects project-id {
            title: title,
            description: description,
            creator: tx-sender,
            status: PROJECT_STATUS_ACTIVE,
            created-at: stacks-block-height,
            finalized-at: none,
            total-contributors: u1,
            content-hash: none,
            license-type: license-type
        })
        
        ;; Add creator as primary contributor
        (map-set contributor-attribution 
            { project-id: project-id, contributor: tx-sender }
            {
                contribution-percentage: u50, ;; Creator starts with 50%
                role: "Project Creator",
                joined-at: stacks-block-height,
                is-active: true,
                earned-credits: u0,
                total-contributions: u0
            }
        )
        
        (var-set next-project-id (+ project-id u1))
        (ok project-id)
    )
)

;; Add contributor to project
(define-public (add-contributor 
    (project-id uint) 
    (contributor principal) 
    (role (string-ascii 50)) 
    (initial-percentage uint))
    (let (
        (project (unwrap! (map-get? collaborative-projects project-id) ERR-PROJECT-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (get creator project)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status project) PROJECT_STATUS_ACTIVE) ERR-PROJECT-FINALIZED)
        (asserts! (<= initial-percentage u30) ERR-INVALID-PERCENTAGE)
        (asserts! (is-none (map-get? contributor-attribution { project-id: project-id, contributor: contributor })) ERR-ALREADY-MEMBER)
        
        (map-set contributor-attribution 
            { project-id: project-id, contributor: contributor }
            {
                contribution-percentage: initial-percentage,
                role: role,
                joined-at: stacks-block-height,
                is-active: true,
                earned-credits: u0,
                total-contributions: u0
            }
        )
        
        (map-set collaborative-projects project-id
            (merge project { total-contributors: (+ (get total-contributors project) u1) })
        )
        
        (ok true)
    )
)

;; Submit contribution to project
(define-public (submit-contribution 
    (project-id uint) 
    (contribution-type (string-ascii 30)) 
    (description (string-utf8 200)) 
    (content-hash (buff 32)) 
    (attribution-weight uint))
    (let (
        (project (unwrap! (map-get? collaborative-projects project-id) ERR-PROJECT-NOT-FOUND))
        (contributor-status (unwrap! (map-get? contributor-attribution { project-id: project-id, contributor: tx-sender }) ERR-NOT-AUTHORIZED))
    )
        (asserts! (get is-active contributor-status) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status project) PROJECT_STATUS_ACTIVE) ERR-PROJECT-FINALIZED)
        (asserts! (is-eq (len content-hash) u32) ERR-INVALID-CONTRIBUTION)
        
        (map-set project-contributions 
            { project-id: project-id, contributor: tx-sender }
            {
                contribution-type: contribution-type,
                description: description,
                content-hash: content-hash,
                timestamp: stacks-block-height,
                attribution-weight: attribution-weight,
                approved: false,
                votes-for: u0,
                votes-against: u0
            }
        )
        
        ;; Update contributor stats
        (map-set contributor-attribution 
            { project-id: project-id, contributor: tx-sender }
            (merge contributor-status { total-contributions: (+ (get total-contributions contributor-status) u1) })
        )
        
        (ok true)
    )
)

;; Vote on contribution approval
(define-public (vote-on-contribution 
    (project-id uint) 
    (contribution-contributor principal) 
    (approve bool))
    (let (
        (voter-status (unwrap! (map-get? contributor-attribution { project-id: project-id, contributor: tx-sender }) ERR-NOT-AUTHORIZED))
        (contribution (unwrap! (map-get? project-contributions { project-id: project-id, contributor: contribution-contributor }) ERR-INVALID-CONTRIBUTION))
        (existing-vote (map-get? contribution-votes { project-id: project-id, contribution-id: u1, voter: tx-sender }))
        (voting-power (get contribution-percentage voter-status))
    )
        (asserts! (get is-active voter-status) ERR-NOT-AUTHORIZED)
        (asserts! (not (get approved contribution)) ERR-VOTING-CLOSED)
        (asserts! (is-none existing-vote) ERR-ALREADY-MEMBER)
        
        ;; Record vote
        (map-set contribution-votes 
            { project-id: project-id, contribution-id: u1, voter: tx-sender }
            {
                vote: approve,
                voting-power: voting-power,
                voted-at: stacks-block-height
            }
        )
        
        ;; Update vote counts
        (map-set project-contributions 
            { project-id: project-id, contributor: contribution-contributor }
            (merge contribution {
                votes-for: (if approve (+ (get votes-for contribution) voting-power) (get votes-for contribution)),
                votes-against: (if approve (get votes-against contribution) (+ (get votes-against contribution) voting-power))
            })
        )
        
        (ok true)
    )
)

;; Finalize project and create final content hash
(define-public (finalize-project (project-id uint) (final-content-hash (buff 32)))
    (let (
        (project (unwrap! (map-get? collaborative-projects project-id) ERR-PROJECT-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (get creator project)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status project) PROJECT_STATUS_ACTIVE) ERR-PROJECT-FINALIZED)
        (asserts! (is-eq (len final-content-hash) u32) ERR-INVALID-CONTRIBUTION)
        
        (map-set collaborative-projects project-id
            (merge project {
                status: PROJECT_STATUS_FINALIZED,
                finalized-at: (some stacks-block-height),
                content-hash: (some final-content-hash)
            })
        )
        
        ;; Initialize revenue pool
        (map-set revenue-pools project-id {
            total-revenue: u0,
            distributed-amount: u0,
            payment-count: u0,
            last-distribution: u0
        })
        
        (ok true)
    )
)

;; Add milestone to project
(define-public (add-milestone 
    (project-id uint) 
    (milestone-id uint) 
    (title (string-ascii 100)) 
    (description (string-utf8 300)) 
    (deadline uint))
    (let (
        (project (unwrap! (map-get? collaborative-projects project-id) ERR-PROJECT-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (get creator project)) ERR-NOT-AUTHORIZED)
        
        (map-set project-milestones 
            { project-id: project-id, milestone-id: milestone-id }
            {
                title: title,
                description: description,
                deadline: deadline,
                status: u0,
                assigned-to: none,
                completion-date: none
            }
        )
        
        (ok true)
    )
)

;; Post project update
(define-public (post-project-update 
    (project-id uint) 
    (message (string-utf8 400)) 
    (update-type (string-ascii 20)))
    (let (
        (contributor-status (unwrap! (map-get? contributor-attribution { project-id: project-id, contributor: tx-sender }) ERR-NOT-AUTHORIZED))
        (update-id (var-get next-update-id))
    )
        (asserts! (get is-active contributor-status) ERR-NOT-AUTHORIZED)
        
        (map-set project-updates 
            { project-id: project-id, update-id: update-id }
            {
                author: tx-sender,
                message: message,
                timestamp: stacks-block-height,
                update-type: update-type
            }
        )
        
        (var-set next-update-id (+ update-id u1))
        (ok update-id)
    )
)

;; Distribute revenue among contributors
(define-public (distribute-revenue (project-id uint) (total-amount uint))
    (let (
        (project (unwrap! (map-get? collaborative-projects project-id) ERR-PROJECT-NOT-FOUND))
        (revenue-pool (unwrap! (map-get? revenue-pools project-id) ERR-PROJECT-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (get creator project)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status project) PROJECT_STATUS_FINALIZED) ERR-PROJECT-FINALIZED)
        
        (map-set revenue-pools project-id
            (merge revenue-pool {
                total-revenue: (+ (get total-revenue revenue-pool) total-amount),
                payment-count: (+ (get payment-count revenue-pool) u1),
                last-distribution: stacks-block-height
            })
        )
        
        (ok true)
    )
)

;; Read-only functions
(define-read-only (get-project (project-id uint))
    (map-get? collaborative-projects project-id)
)

(define-read-only (get-contributor-info (project-id uint) (contributor principal))
    (map-get? contributor-attribution { project-id: project-id, contributor: contributor })
)

(define-read-only (get-contribution (project-id uint) (contributor principal))
    (map-get? project-contributions { project-id: project-id, contributor: contributor })
)

(define-read-only (get-milestone (project-id uint) (milestone-id uint))
    (map-get? project-milestones { project-id: project-id, milestone-id: milestone-id })
)

(define-read-only (get-project-update (project-id uint) (update-id uint))
    (map-get? project-updates { project-id: project-id, update-id: update-id })
)

(define-read-only (get-revenue-pool (project-id uint))
    (map-get? revenue-pools project-id)
)

(define-read-only (calculate-contributor-share (project-id uint) (contributor principal) (amount uint))
    (let (
        (attribution (map-get? contributor-attribution { project-id: project-id, contributor: contributor }))
    )
        (match attribution
            contributor-data (some (/ (* amount (get contribution-percentage contributor-data)) u100))
            none
        )
    )
)
