(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_ALREADY_EXISTS (err u101))
(define-constant ERR_NOT_FOUND (err u102))
(define-constant ERR_INVALID_HASH (err u103))
(define-constant ERR_INVALID_TITLE (err u104))

(define-data-var next-work-id uint u1)

(define-map works
  { work-id: uint }
  {
    author: principal,
    title: (string-ascii 100),
    content-hash: (buff 32),
    timestamp: uint,
    stacks-block-height: uint,
    description: (string-utf8 500)
  }
)

(define-map author-works
  { author: principal, work-index: uint }
  { work-id: uint }
)

(define-map author-work-count
  { author: principal }
  { count: uint }
)

(define-map content-hash-to-work
  { content-hash: (buff 32) }
  { work-id: uint }
)

(define-public (register-work (title (string-ascii 100)) (content-hash (buff 32)) (description (string-utf8 500)))
  (let
    (
      (work-id (var-get next-work-id))
      (author tx-sender)
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
      (author-count (default-to u0 (get count (map-get? author-work-count { author: author }))))
    )
    (asserts! (> (len title) u0) ERR_INVALID_TITLE)
    (asserts! (is-eq (len content-hash) u32) ERR_INVALID_HASH)
    (asserts! (is-none (map-get? content-hash-to-work { content-hash: content-hash })) ERR_ALREADY_EXISTS)
    
    (map-set works
      { work-id: work-id }
      {
        author: author,
        title: title,
        content-hash: content-hash,
        timestamp: current-time,
        stacks-block-height: stacks-block-height,
        description: description
      }
    )
    
    (map-set author-works
      { author: author, work-index: author-count }
      { work-id: work-id }
    )
    
    (map-set author-work-count
      { author: author }
      { count: (+ author-count u1) }
    )
    
    (map-set content-hash-to-work
      { content-hash: content-hash }
      { work-id: work-id }
    )
    
    (var-set next-work-id (+ work-id u1))
    (ok work-id)
  )
)

(define-public (transfer-authorship (work-id uint) (new-author principal))
  (let
    (
      (work (unwrap! (map-get? works { work-id: work-id }) ERR_NOT_FOUND))
      (current-author (get author work))
    )
    (asserts! (is-eq tx-sender current-author) ERR_NOT_AUTHORIZED)
    
    (map-set works
      { work-id: work-id }
      (merge work { author: new-author })
    )
    (ok true)
  )
)

(define-public (update-work-description (work-id uint) (new-description (string-utf8 500)))
  (let
    (
      (work (unwrap! (map-get? works { work-id: work-id }) ERR_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get author work)) ERR_NOT_AUTHORIZED)
    
    (map-set works
      { work-id: work-id }
      (merge work { description: new-description })
    )
    (ok true)
  )
)

(define-read-only (get-work (work-id uint))
  (map-get? works { work-id: work-id })
)

(define-read-only (get-work-by-hash (content-hash (buff 32)))
  (match (map-get? content-hash-to-work { content-hash: content-hash })
    work-entry (get-work (get work-id work-entry))
    none
  )
)

(define-read-only (verify-authorship (work-id uint) (author principal))
  (match (get-work work-id)
    work (is-eq (get author work) author)
    false
  )
)

(define-read-only (verify-authorship-by-hash (content-hash (buff 32)) (author principal))
  (match (get-work-by-hash content-hash)
    work (is-eq (get author work) author)
    false
  )
)

(define-read-only (get-author-work-count (author principal))
  (default-to u0 (get count (map-get? author-work-count { author: author })))
)

(define-read-only (get-author-work-by-index (author principal) (index uint))
  (match (map-get? author-works { author: author, work-index: index })
    work-entry (get-work (get work-id work-entry))
    none
  )
)

(define-read-only (get-total-works)
  (- (var-get next-work-id) u1)
)

(define-read-only (is-content-registered (content-hash (buff 32)))
  (is-some (map-get? content-hash-to-work { content-hash: content-hash }))
)

(define-read-only (get-work-timestamp (work-id uint))
  (match (get-work work-id)
    work (some (get timestamp work))
    none
  )
)

(define-read-only (get-work-stacks-block-height (work-id uint))
  (match (get-work work-id)
    work (some (get stacks-block-height work))
    none
  )
)

(define-read-only (check-plagiarism (content-hash (buff 32)) (claimed-author principal))
  (match (get-work-by-hash content-hash)
    existing-work 
      (if (is-eq (get author existing-work) claimed-author)
        { is-plagiarism: false, original-author: claimed-author, registration-time: (get timestamp existing-work) }
        { is-plagiarism: true, original-author: (get author existing-work), registration-time: (get timestamp existing-work) }
      )
    { is-plagiarism: false, original-author: claimed-author, registration-time: u0 }
  )
)

(define-constant ERR_INVALID_LICENSE_TYPE (err u105))
(define-constant ERR_INVALID_PRICE (err u106))
(define-constant ERR_LICENSE_NOT_FOUND (err u107))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u108))
(define-constant ERR_LICENSE_EXPIRED (err u109))
(define-constant ERR_INVALID_DURATION (err u110))
(define-constant ERR_INVALID_REVENUE_SHARE (err u111))
(define-constant ERR_COLLABORATOR_NOT_FOUND (err u112))

(define-constant LICENSE_TYPE_PERSONAL u1)
(define-constant LICENSE_TYPE_COMMERCIAL u2)
(define-constant LICENSE_TYPE_EXCLUSIVE u3)
(define-constant LICENSE_TYPE_UNLIMITED u4)

(define-data-var next-license-id uint u1)

(define-map work-licenses
  { work-id: uint, license-type: uint }
  {
    price: uint,
    duration-blocks: uint,
    max-uses: uint,
    is-active: bool,
    created-at: uint
  }
)

(define-map license-purchases
  { license-id: uint }
  {
    work-id: uint,
    license-type: uint,
    licensee: principal,
    purchase-price: uint,
    purchase-block: uint,
    expiry-block: uint,
    uses-remaining: uint,
    is-active: bool
  }
)

(define-map work-collaborators
  { work-id: uint, collaborator: principal }
  {
    revenue-share: uint,
    role: (string-ascii 50),
    added-at: uint
  }
)

(define-map license-usage-stats
  { work-id: uint }
  {
    total-revenue: uint,
    total-licenses-sold: uint,
    personal-licenses: uint,
    commercial-licenses: uint,
    exclusive-licenses: uint,
    unlimited-licenses: uint
  }
)

(define-map user-license-history
  { user: principal, license-index: uint }
  { license-id: uint }
)

(define-map user-license-count
  { user: principal }
  { count: uint }
)

(define-public (create-license-tier (work-id uint) (license-type uint) (price uint) (duration-blocks uint) (max-uses uint))
  (let
    (
      (work (unwrap! (map-get? works { work-id: work-id }) ERR_NOT_FOUND))
      (author (get author work))
    )
    (asserts! (is-eq tx-sender author) ERR_NOT_AUTHORIZED)
    (asserts! (and (>= license-type u1) (<= license-type u4)) ERR_INVALID_LICENSE_TYPE)
    (asserts! (> price u0) ERR_INVALID_PRICE)
    (asserts! (> duration-blocks u0) ERR_INVALID_DURATION)
    
    (map-set work-licenses
      { work-id: work-id, license-type: license-type }
      {
        price: price,
        duration-blocks: duration-blocks,
        max-uses: max-uses,
        is-active: true,
        created-at: stacks-block-height
      }
    )
    (ok true)
  )
)

(define-public (purchase-license (work-id uint) (license-type uint))
  (let
    (
      (license-config (unwrap! (map-get? work-licenses { work-id: work-id, license-type: license-type }) ERR_LICENSE_NOT_FOUND))
      (license-id (var-get next-license-id))
      (purchaser tx-sender)
      (price (get price license-config))
      (duration (get duration-blocks license-config))
      (max-uses (get max-uses license-config))
      (user-count (default-to u0 (get count (map-get? user-license-count { user: purchaser }))))
      (current-stats (default-to 
        { total-revenue: u0, total-licenses-sold: u0, personal-licenses: u0, commercial-licenses: u0, exclusive-licenses: u0, unlimited-licenses: u0 }
        (map-get? license-usage-stats { work-id: work-id })))
    )
    (asserts! (get is-active license-config) ERR_LICENSE_NOT_FOUND)
    (asserts! (>= (stx-get-balance purchaser) price) ERR_INSUFFICIENT_PAYMENT)
    
    (unwrap! (stx-transfer? price purchaser (get author (unwrap-panic (get-work work-id)))) ERR_INSUFFICIENT_PAYMENT)
    
    (map-set license-purchases
      { license-id: license-id }
      {
        work-id: work-id,
        license-type: license-type,
        licensee: purchaser,
        purchase-price: price,
        purchase-block: stacks-block-height,
        expiry-block: (+ stacks-block-height duration),
        uses-remaining: max-uses,
        is-active: true
      }
    )
    
    (map-set user-license-history
      { user: purchaser, license-index: user-count }
      { license-id: license-id }
    )
    
    (map-set user-license-count
      { user: purchaser }
      { count: (+ user-count u1) }
    )
    
    (map-set license-usage-stats
      { work-id: work-id }
      {
        total-revenue: (+ (get total-revenue current-stats) price),
        total-licenses-sold: (+ (get total-licenses-sold current-stats) u1),
        personal-licenses: (+ (get personal-licenses current-stats) (if (is-eq license-type LICENSE_TYPE_PERSONAL) u1 u0)),
        commercial-licenses: (+ (get commercial-licenses current-stats) (if (is-eq license-type LICENSE_TYPE_COMMERCIAL) u1 u0)),
        exclusive-licenses: (+ (get exclusive-licenses current-stats) (if (is-eq license-type LICENSE_TYPE_EXCLUSIVE) u1 u0)),
        unlimited-licenses: (+ (get unlimited-licenses current-stats) (if (is-eq license-type LICENSE_TYPE_UNLIMITED) u1 u0))
      }
    )
    
    (var-set next-license-id (+ license-id u1))
    (ok license-id)
  )
)

(define-public (use-license (license-id uint))
  (let
    (
      (license (unwrap! (map-get? license-purchases { license-id: license-id }) ERR_LICENSE_NOT_FOUND))
      (licensee (get licensee license))
    )
    (asserts! (is-eq tx-sender licensee) ERR_NOT_AUTHORIZED)
    (asserts! (get is-active license) ERR_LICENSE_EXPIRED)
    (asserts! (< stacks-block-height (get expiry-block license)) ERR_LICENSE_EXPIRED)
    (asserts! (> (get uses-remaining license) u0) ERR_LICENSE_EXPIRED)
    
    (map-set license-purchases
      { license-id: license-id }
      (merge license { uses-remaining: (- (get uses-remaining license) u1) })
    )
    (ok true)
  )
)

(define-public (revoke-license (license-id uint))
  (let
    (
      (license (unwrap! (map-get? license-purchases { license-id: license-id }) ERR_LICENSE_NOT_FOUND))
      (work-id (get work-id license))
      (work (unwrap! (map-get? works { work-id: work-id }) ERR_NOT_FOUND))
      (author (get author work))
    )
    (asserts! (is-eq tx-sender author) ERR_NOT_AUTHORIZED)
    
    (map-set license-purchases
      { license-id: license-id }
      (merge license { is-active: false })
    )
    (ok true)
  )
)

(define-public (deactivate-license-tier (work-id uint) (license-type uint))
  (let
    (
      (work (unwrap! (map-get? works { work-id: work-id }) ERR_NOT_FOUND))
      (license-config (unwrap! (map-get? work-licenses { work-id: work-id, license-type: license-type }) ERR_LICENSE_NOT_FOUND))
      (author (get author work))
    )
    (asserts! (is-eq tx-sender author) ERR_NOT_AUTHORIZED)
    
    (map-set work-licenses
      { work-id: work-id, license-type: license-type }
      (merge license-config { is-active: false })
    )
    (ok true)
  )
)

(define-public (add-collaborator (work-id uint) (collaborator principal) (revenue-share uint) (role (string-ascii 50)))
  (let
    (
      (work (unwrap! (map-get? works { work-id: work-id }) ERR_NOT_FOUND))
      (author (get author work))
    )
    (asserts! (is-eq tx-sender author) ERR_NOT_AUTHORIZED)
    (asserts! (and (> revenue-share u0) (<= revenue-share u100)) ERR_INVALID_REVENUE_SHARE)
    
    (map-set work-collaborators
      { work-id: work-id, collaborator: collaborator }
      {
        revenue-share: revenue-share,
        role: role,
        added-at: stacks-block-height
      }
    )
    (ok true)
  )
)

(define-public (remove-collaborator (work-id uint) (collaborator principal))
  (let
    (
      (work (unwrap! (map-get? works { work-id: work-id }) ERR_NOT_FOUND))
      (author (get author work))
    )
    (asserts! (is-eq tx-sender author) ERR_NOT_AUTHORIZED)
    (asserts! (is-some (map-get? work-collaborators { work-id: work-id, collaborator: collaborator })) ERR_COLLABORATOR_NOT_FOUND)
    
    (map-delete work-collaborators { work-id: work-id, collaborator: collaborator })
    (ok true)
  )
)

(define-read-only (get-license-config (work-id uint) (license-type uint))
  (map-get? work-licenses { work-id: work-id, license-type: license-type })
)

(define-read-only (get-license-details (license-id uint))
  (map-get? license-purchases { license-id: license-id })
)

(define-read-only (validate-license-usage (license-id uint) (user principal))
  (match (map-get? license-purchases { license-id: license-id })
    license
      (and
        (is-eq (get licensee license) user)
        (get is-active license)
        (< stacks-block-height (get expiry-block license))
        (> (get uses-remaining license) u0)
      )
    false
  )
)

(define-read-only (get-work-revenue-stats (work-id uint))
  (map-get? license-usage-stats { work-id: work-id })
)

(define-read-only (get-collaborator-info (work-id uint) (collaborator principal))
  (map-get? work-collaborators { work-id: work-id, collaborator: collaborator })
)

(define-read-only (get-user-license-count (user principal))
  (default-to u0 (get count (map-get? user-license-count { user: user })))
)

(define-read-only (get-user-license-by-index (user principal) (index uint))
  (match (map-get? user-license-history { user: user, license-index: index })
    license-entry (get-license-details (get license-id license-entry))
    none
  )
)

(define-read-only (check-license-expiry (license-id uint))
  (match (map-get? license-purchases { license-id: license-id })
    license
      {
        is-expired: (>= stacks-block-height (get expiry-block license)),
        blocks-remaining: (if (>= stacks-block-height (get expiry-block license)) u0 (- (get expiry-block license) stacks-block-height)),
        uses-remaining: (get uses-remaining license)
      }
    { is-expired: true, blocks-remaining: u0, uses-remaining: u0 }
  )
)

(define-read-only (get-work-license-types (work-id uint))
  (list
    (get-license-config work-id LICENSE_TYPE_PERSONAL)
    (get-license-config work-id LICENSE_TYPE_COMMERCIAL)
    (get-license-config work-id LICENSE_TYPE_EXCLUSIVE)
    (get-license-config work-id LICENSE_TYPE_UNLIMITED)
  )
)

(define-constant ERR_INVALID_VERSION_NUMBER (err u113))
(define-constant ERR_VERSION_NOT_FOUND (err u114))
(define-constant ERR_INVALID_PARENT_WORK (err u115))
(define-constant ERR_CIRCULAR_REFERENCE (err u116))
(define-constant ERR_UNAUTHORIZED_VERSION (err u117))
(define-constant ERR_INVALID_MODIFICATION_TYPE (err u118))

(define-constant MODIFICATION_TYPE_REVISION u1)
(define-constant MODIFICATION_TYPE_DERIVATIVE u2)
(define-constant MODIFICATION_TYPE_TRANSLATION u3)
(define-constant MODIFICATION_TYPE_ADAPTATION u4)
(define-constant MODIFICATION_TYPE_COMPILATION u5)

(define-data-var next-version-id uint u1)

(define-map work-versions
  { work-id: uint, version-number: uint }
  {
    version-id: uint,
    author: principal,
    content-hash: (buff 32),
    modification-type: uint,
    modification-description: (string-utf8 300),
    timestamp: uint,
    stacks-block-height: uint,
    is-active: bool
  }
)

(define-map work-version-count
  { work-id: uint }
  { count: uint }
)

(define-map derivative-works
  { derivative-work-id: uint }
  {
    parent-work-id: uint,
    derivative-type: uint,
    attribution-percentage: uint,
    derivation-description: (string-utf8 400),
    registered-at: uint,
    is-approved: bool
  }
)

(define-map work-genealogy
  { work-id: uint }
  {
    root-work-id: uint,
    parent-work-id: uint,
    generation-level: uint,
    total-derivatives: uint,
    lineage-path: (list 10 uint)
  }
)

(define-map version-history
  { author: principal, version-index: uint }
  { version-id: uint }
)

(define-map author-version-count
  { author: principal }
  { count: uint }
)

(define-map work-timeline
  { work-id: uint, timeline-index: uint }
  {
    event-type: uint,
    version-id: uint,
    timestamp: uint,
    description: (string-utf8 200)
  }
)

(define-map work-timeline-count
  { work-id: uint }
  { count: uint }
)

(define-public (create-work-version (work-id uint) (content-hash (buff 32)) (modification-type uint) (modification-description (string-utf8 300)))
  (let
    (
      (work (unwrap! (map-get? works { work-id: work-id }) ERR_NOT_FOUND))
      (author (get author work))
      (version-id (var-get next-version-id))
      (current-version-count (default-to u0 (get count (map-get? work-version-count { work-id: work-id }))))
      (new-version-number (+ current-version-count u1))
      (author-count (default-to u0 (get count (map-get? author-version-count { author: tx-sender }))))
      (timeline-count (default-to u0 (get count (map-get? work-timeline-count { work-id: work-id }))))
    )
    (asserts! (is-eq tx-sender author) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (len content-hash) u32) ERR_INVALID_HASH)
    (asserts! (and (>= modification-type u1) (<= modification-type u5)) ERR_INVALID_MODIFICATION_TYPE)
    (asserts! (is-none (map-get? content-hash-to-work { content-hash: content-hash })) ERR_ALREADY_EXISTS)
    
    (map-set work-versions
      { work-id: work-id, version-number: new-version-number }
      {
        version-id: version-id,
        author: tx-sender,
        content-hash: content-hash,
        modification-type: modification-type,
        modification-description: modification-description,
        timestamp: (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))),
        stacks-block-height: stacks-block-height,
        is-active: true
      }
    )
    
    (map-set work-version-count
      { work-id: work-id }
      { count: new-version-number }
    )
    
    (map-set version-history
      { author: tx-sender, version-index: author-count }
      { version-id: version-id }
    )
    
    (map-set author-version-count
      { author: tx-sender }
      { count: (+ author-count u1) }
    )
    
    (map-set work-timeline
      { work-id: work-id, timeline-index: timeline-count }
      {
        event-type: u1,
        version-id: version-id,
        timestamp: (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))),
        description: (unwrap-panic (as-max-len? modification-description u200))
      }
    )
    
    (map-set work-timeline-count
      { work-id: work-id }
      { count: (+ timeline-count u1) }
    )
    
    (var-set next-version-id (+ version-id u1))
    (ok version-id)
  )
)

(define-public (register-derivative-work (title (string-ascii 100)) (content-hash (buff 32)) (description (string-utf8 500)) (parent-work-id uint) (derivative-type uint) (attribution-percentage uint) (derivation-description (string-utf8 400)))
  (let
    (
      (parent-work (unwrap! (map-get? works { work-id: parent-work-id }) ERR_INVALID_PARENT_WORK))
      (work-id (var-get next-work-id))
      (author tx-sender)
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
      (author-count (default-to u0 (get count (map-get? author-work-count { author: author }))))
      (parent-genealogy (map-get? work-genealogy { work-id: parent-work-id }))
      (timeline-count (default-to u0 (get count (map-get? work-timeline-count { work-id: parent-work-id }))))
    )
    (asserts! (> (len title) u0) ERR_INVALID_TITLE)
    (asserts! (is-eq (len content-hash) u32) ERR_INVALID_HASH)
    (asserts! (and (>= derivative-type u1) (<= derivative-type u5)) ERR_INVALID_MODIFICATION_TYPE)
    (asserts! (and (> attribution-percentage u0) (<= attribution-percentage u100)) ERR_INVALID_REVENUE_SHARE)
    (asserts! (is-none (map-get? content-hash-to-work { content-hash: content-hash })) ERR_ALREADY_EXISTS)
    
    (unwrap! (register-work title content-hash description) ERR_ALREADY_EXISTS)
    
    (map-set derivative-works
      { derivative-work-id: work-id }
      {
        parent-work-id: parent-work-id,
        derivative-type: derivative-type,
        attribution-percentage: attribution-percentage,
        derivation-description: derivation-description,
        registered-at: current-time,
        is-approved: false
      }
    )
    
    (match parent-genealogy
      genealogy
        (map-set work-genealogy
          { work-id: work-id }
          {
            root-work-id: (get root-work-id genealogy),
            parent-work-id: parent-work-id,
            generation-level: (+ (get generation-level genealogy) u1),
            total-derivatives: u0,
            lineage-path: (unwrap-panic (as-max-len? (append (get lineage-path genealogy) parent-work-id) u10))
          }
        )
      (map-set work-genealogy
        { work-id: work-id }
        {
          root-work-id: parent-work-id,
          parent-work-id: parent-work-id,
          generation-level: u1,
          total-derivatives: u0,
          lineage-path: (list parent-work-id)
        }
      )
    )
    
    (map-set work-timeline
      { work-id: parent-work-id, timeline-index: timeline-count }
      {
        event-type: u2,
        version-id: work-id,
        timestamp: current-time,
        description: (unwrap-panic (as-max-len? derivation-description u200))
      }
    )
    
    (map-set work-timeline-count
      { work-id: parent-work-id }
      { count: (+ timeline-count u1) }
    )
    
    (ok work-id)
  )
)

(define-public (approve-derivative-work (derivative-work-id uint))
  (let
    (
      (derivative (unwrap! (map-get? derivative-works { derivative-work-id: derivative-work-id }) ERR_NOT_FOUND))
      (parent-work-id (get parent-work-id derivative))
      (parent-work (unwrap! (map-get? works { work-id: parent-work-id }) ERR_NOT_FOUND))
      (parent-author (get author parent-work))
    )
    (asserts! (is-eq tx-sender parent-author) ERR_NOT_AUTHORIZED)
    
    (map-set derivative-works
      { derivative-work-id: derivative-work-id }
      (merge derivative { is-approved: true })
    )
    (ok true)
  )
)

(define-public (deactivate-version (work-id uint) (version-number uint))
  (let
    (
      (work (unwrap! (map-get? works { work-id: work-id }) ERR_NOT_FOUND))
      (version (unwrap! (map-get? work-versions { work-id: work-id, version-number: version-number }) ERR_VERSION_NOT_FOUND))
      (author (get author work))
    )
    (asserts! (is-eq tx-sender author) ERR_NOT_AUTHORIZED)
    
    (map-set work-versions
      { work-id: work-id, version-number: version-number }
      (merge version { is-active: false })
    )
    (ok true)
  )
)

(define-read-only (get-work-version (work-id uint) (version-number uint))
  (map-get? work-versions { work-id: work-id, version-number: version-number })
)

(define-read-only (get-work-version-count (work-id uint))
  (default-to u0 (get count (map-get? work-version-count { work-id: work-id })))
)

(define-read-only (get-derivative-work-info (derivative-work-id uint))
  (map-get? derivative-works { derivative-work-id: derivative-work-id })
)

(define-read-only (get-work-genealogy (work-id uint))
  (map-get? work-genealogy { work-id: work-id })
)

(define-read-only (get-work-timeline-entry (work-id uint) (timeline-index uint))
  (map-get? work-timeline { work-id: work-id, timeline-index: timeline-index })
)

(define-read-only (get-work-timeline-count (work-id uint))
  (default-to u0 (get count (map-get? work-timeline-count { work-id: work-id })))
)

(define-read-only (get-author-version-count (author principal))
  (default-to u0 (get count (map-get? author-version-count { author: author })))
)

(define-read-only (get-author-version-by-index (author principal) (index uint))
  (match (map-get? version-history { author: author, version-index: index })
    version-entry (some (get version-id version-entry))
    none
  )
)

(define-read-only (verify-work-lineage (work-id uint) (claimed-root uint))
  (match (get-work-genealogy work-id)
    genealogy (is-eq (get root-work-id genealogy) claimed-root)
    false
  )
)

(define-read-only (get-generation-level (work-id uint))
  (match (get-work-genealogy work-id)
    genealogy (get generation-level genealogy)
    u0
  )
)

(define-read-only (check-derivative-approval (derivative-work-id uint))
  (match (get-derivative-work-info derivative-work-id)
    derivative (get is-approved derivative)
    false
  )
)

(define-read-only (get-work-evolution-stats (work-id uint))
  {
    total-versions: (get-work-version-count work-id),
    timeline-events: (get-work-timeline-count work-id),
    generation-level: (get-generation-level work-id),
    has-derivatives: (> (match (get-work-genealogy work-id) genealogy (get total-derivatives genealogy) u0) u0)
  }
)

(define-read-only (compare-versions (work-id uint) (version-a uint) (version-b uint))
  (let
    (
      (version-a-data (get-work-version work-id version-a))
      (version-b-data (get-work-version work-id version-b))
    )
    (match version-a-data
      va (match version-b-data
        vb (some {
          version-a-hash: (get content-hash va),
          version-b-hash: (get content-hash vb),
          time-difference: (if (> (get timestamp va) (get timestamp vb)) (- (get timestamp va) (get timestamp vb)) (- (get timestamp vb) (get timestamp va))),
          modification-types: { a: (get modification-type va), b: (get modification-type vb) },
          same-author: (is-eq (get author va) (get author vb))
        })
        none
      )
      none
    )
  )
)



