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