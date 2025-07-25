;; BookChain: Decentralized Digital Library Platform
;; Version: 1.0.0
;; Manage and distribute digital books with author rights on-chain

;; Library Statistics
(define-data-var total-books uint u0)

;; Core Library Data
(define-map book-registry
  { book-id: uint }
  {
    title: (string-ascii 64),
    author: principal,
    pages: uint,
    publication-block: uint,
    summary: (string-ascii 128),
    categories: (list 10 (string-ascii 32))
  })

(define-map reading-permissions
  { book-id: uint, reader: principal }
  { can-read: bool })

;; Library Error Codes
(define-constant book-not-found (err u401))
(define-constant book-already-exists (err u402))
(define-constant invalid-title (err u403))
(define-constant invalid-pages (err u404))
(define-constant unauthorized-access (err u405))
(define-constant not-author (err u406))
(define-constant admin-only (err u400))
(define-constant reading-restricted (err u407))
(define-constant invalid-categories (err u408))

;; Library Administrator
(define-constant library-admin tx-sender)

;; ===== Library Helper Functions =====

;; Check if book exists in library
(define-private (book-exists (book-id uint))
  (is-some (map-get? book-registry { book-id: book-id })))

;; Verify author ownership
(define-private (is-author (book-id uint) (caller principal))
  (match (map-get? book-registry { book-id: book-id })
    book-data (is-eq (get author book-data) caller)
    false
  ))

;; Get book page count
(define-private (get-page-count (book-id uint))
  (default-to u0
    (get pages
      (map-get? book-registry { book-id: book-id })
    )
  ))

;; Validate category format
(define-private (is-valid-category (category (string-ascii 32)))
  (and
    (> (len category) u0)
    (< (len category) u33)
  ))

;; Validate all categories in collection
(define-private (validate-categories (categories (list 10 (string-ascii 32))))
  (and
    (> (len categories) u0)
    (<= (len categories) u10)
    (is-eq (len (filter is-valid-category categories)) (len categories))
  ))

;; ===== Library Management Functions =====

;; Add new book to library
(define-public (add-book
  (title (string-ascii 64))
  (pages uint)
  (summary (string-ascii 128))
  (categories (list 10 (string-ascii 32))))
  (let
    (
      (next-id (+ (var-get total-books) u1))
    )
    ;; Validate inputs
    (asserts! (> (len title) u0) invalid-title)
    (asserts! (< (len title) u65) invalid-title)
    (asserts! (> pages u0) invalid-pages)
    (asserts! (< pages u10000) invalid-pages)
    (asserts! (> (len summary) u0) invalid-title)
    (asserts! (< (len summary) u129) invalid-title)
    (asserts! (validate-categories categories) invalid-categories)
    
    ;; Register book
    (map-insert book-registry
      { book-id: next-id }
      {
        title: title,
        author: tx-sender,
        pages: pages,
        publication-block: stacks-block-height,
        summary: summary,
        categories: categories
      }
    )
    
    ;; Grant author reading permission
    (map-insert reading-permissions
      { book-id: next-id, reader: tx-sender }
      { can-read: true }
    )
    
    ;; Update counter
    (var-set total-books next-id)
    (ok next-id)
  ))

;; Update existing book details
(define-public (update-book
  (book-id uint)
  (new-title (string-ascii 64))
  (new-pages uint)
  (new-summary (string-ascii 128))
  (new-categories (list 10 (string-ascii 32))))
  (let
    (
      (book-data (unwrap! (map-get? book-registry { book-id: book-id }) book-not-found))
    )
    ;; Verify permissions and inputs
    (asserts! (book-exists book-id) book-not-found)
    (asserts! (is-eq (get author book-data) tx-sender) not-author)
    (asserts! (> (len new-title) u0) invalid-title)
    (asserts! (< (len new-title) u65) invalid-title)
    (asserts! (> new-pages u0) invalid-pages)
    (asserts! (< new-pages u10000) invalid-pages)
    (asserts! (> (len new-summary) u0) invalid-title)
    (asserts! (< (len new-summary) u129) invalid-title)
    (asserts! (validate-categories new-categories) invalid-categories)
    
    ;; Update book information
    (map-set book-registry
      { book-id: book-id }
      (merge book-data {
        title: new-title,
        pages: new-pages,
        summary: new-summary,
        categories: new-categories
      })
    )
    (ok true)
  ))

;; Remove book from library
(define-public (remove-book (book-id uint))
  (let
    (
      (book-data (unwrap! (map-get? book-registry { book-id: book-id }) book-not-found))
    )
    ;; Verify author ownership
    (asserts! (book-exists book-id) book-not-found)
    (asserts! (is-eq (get author book-data) tx-sender) not-author)
    
    ;; Remove from library
    (map-delete book-registry { book-id: book-id })
    (ok true)
  ))

;; Transfer book to new author
(define-public (transfer-book (book-id uint) (new-author principal))
  (let
    (
      (book-data (unwrap! (map-get? book-registry { book-id: book-id }) book-not-found))
    )
    ;; Verify current author
    (asserts! (book-exists book-id) book-not-found)
    (asserts! (is-eq (get author book-data) tx-sender) not-author)
    
    ;; Transfer ownership
    (map-set book-registry
      { book-id: book-id }
      (merge book-data { author: new-author })
    )
    (ok true)
  ))

;; ===== Read-Only Functions =====

;; Get total books in library
(define-read-only (get-total-books)
  (var-get total-books))

;; Get book details
(define-read-only (get-book-info (book-id uint))
  (map-get? book-registry { book-id: book-id }))

;; Get reading permission
(define-read-only (get-reading-permission (book-id uint) (reader principal))
  (map-get? reading-permissions { book-id: book-id, reader: reader }))

;; Get library stats
(define-read-only (get-library-stats)
  {
    admin: library-admin,
    total-books: (var-get total-books)
  })