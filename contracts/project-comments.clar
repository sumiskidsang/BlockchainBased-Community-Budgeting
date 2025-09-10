;; Project Comments & Feedback System
;; Allows citizens to comment on project proposals for better community engagement

;; Error constants
(define-constant ERR_UNAUTHORIZED (err u200))
(define-constant ERR_PROJECT_NOT_FOUND (err u201))
(define-constant ERR_COMMENT_NOT_FOUND (err u202))
(define-constant ERR_COMMENT_TOO_LONG (err u203))
(define-constant ERR_INVALID_RATING (err u204))
(define-constant ERR_ALREADY_RATED (err u205))

;; Comment types
(define-constant COMMENT_TYPE_FEEDBACK u1)
(define-constant COMMENT_TYPE_QUESTION u2)
(define-constant COMMENT_TYPE_SUGGESTION u3)
(define-constant COMMENT_TYPE_SUPPORT u4)

;; Data structures
(define-data-var comment-count uint u0)

(define-map project-comments uint
  {
    project-id: uint,
    commenter: principal,
    content: (string-ascii 300),
    comment-type: uint,
    timestamp: uint,
    upvotes: uint,
    downvotes: uint,
    active: bool
  }
)

(define-map comment-votes
  {
    comment-id: uint,
    voter: principal
  }
  {
    vote-type: uint  ;; u1 for upvote, u2 for downvote
  }
)

(define-map project-ratings
  {
    project-id: uint,
    rater: principal
  }
  {
    rating: uint,  ;; 1-5 star rating
    timestamp: uint
  }
)

(define-map project-rating-summary uint
  {
    total-ratings: uint,
    average-rating: uint,
    sum-ratings: uint
  }
)

;; Public functions

;; Add a comment to a project
(define-public (add-comment 
  (project-id uint) 
  (content (string-ascii 300)) 
  (comment-type uint))
  (let
    (
      (comment-id (+ (var-get comment-count) u1))
    )
    (asserts! (is-registered-citizen) ERR_UNAUTHORIZED)
    (asserts! (project-exists project-id) ERR_PROJECT_NOT_FOUND)
    (asserts! (> (len content) u0) ERR_COMMENT_TOO_LONG)
    (asserts! (or 
      (is-eq comment-type COMMENT_TYPE_FEEDBACK)
      (is-eq comment-type COMMENT_TYPE_QUESTION)
      (is-eq comment-type COMMENT_TYPE_SUGGESTION)
      (is-eq comment-type COMMENT_TYPE_SUPPORT)
    ) ERR_UNAUTHORIZED)
    
    (var-set comment-count comment-id)
    (map-set project-comments comment-id {
      project-id: project-id,
      commenter: tx-sender,
      content: content,
      comment-type: comment-type,
      timestamp: stacks-block-height,
      upvotes: u0,
      downvotes: u0,
      active: true
    })
    
    (ok comment-id)
  )
)

;; Vote on a comment (upvote/downvote)
(define-public (vote-on-comment (comment-id uint) (vote-type uint))
  (let
    (
      (comment (unwrap! (map-get? project-comments comment-id) ERR_COMMENT_NOT_FOUND))
      (vote-key {comment-id: comment-id, voter: tx-sender})
      (existing-vote (map-get? comment-votes vote-key))
    )
    (asserts! (is-registered-citizen) ERR_UNAUTHORIZED)
    (asserts! (get active comment) ERR_COMMENT_NOT_FOUND)
    (asserts! (or (is-eq vote-type u1) (is-eq vote-type u2)) ERR_UNAUTHORIZED)
    (asserts! (not (is-eq tx-sender (get commenter comment))) ERR_UNAUTHORIZED)
    
    ;; Remove previous vote if exists
    (match existing-vote
      prev-vote
        (begin
          (if (is-eq (get vote-type prev-vote) u1)
            (map-set project-comments comment-id 
              (merge comment { upvotes: (- (get upvotes comment) u1) }))
            (map-set project-comments comment-id 
              (merge comment { downvotes: (- (get downvotes comment) u1) }))
          )
        )
      true
    )
    
    ;; Add new vote
    (map-set comment-votes vote-key { vote-type: vote-type })
    (if (is-eq vote-type u1)
      (map-set project-comments comment-id 
        (merge comment { upvotes: (+ (get upvotes comment) u1) }))
      (map-set project-comments comment-id 
        (merge comment { downvotes: (+ (get downvotes comment) u1) }))
    )
    
    (ok true)
  )
)

;; Rate a project (1-5 stars)
(define-public (rate-project (project-id uint) (rating uint))
  (let
    (
      (rating-key {project-id: project-id, rater: tx-sender})
      (existing-rating (map-get? project-ratings rating-key))
      (current-summary (default-to 
        { total-ratings: u0, average-rating: u0, sum-ratings: u0 }
        (map-get? project-rating-summary project-id)
      ))
    )
    (asserts! (is-registered-citizen) ERR_UNAUTHORIZED)
    (asserts! (project-exists project-id) ERR_PROJECT_NOT_FOUND)
    (asserts! (and (>= rating u1) (<= rating u5)) ERR_INVALID_RATING)
    (asserts! (is-none existing-rating) ERR_ALREADY_RATED)
    
    ;; Add rating
    (map-set project-ratings rating-key {
      rating: rating,
      timestamp: stacks-block-height
    })
    
    ;; Update summary
    (let
      (
        (new-total (+ (get total-ratings current-summary) u1))
        (new-sum (+ (get sum-ratings current-summary) rating))
        (new-average (/ new-sum new-total))
      )
      (map-set project-rating-summary project-id {
        total-ratings: new-total,
        average-rating: new-average,
        sum-ratings: new-sum
      })
    )
    
    (ok true)
  )
)

;; Delete a comment (only by commenter or admin)
(define-public (delete-comment (comment-id uint))
  (let
    (
      (comment (unwrap! (map-get? project-comments comment-id) ERR_COMMENT_NOT_FOUND))
    )
    (asserts! (or 
      (is-eq tx-sender (get commenter comment))
      (is-admin-user)
    ) ERR_UNAUTHORIZED)
    
    (map-set project-comments comment-id 
      (merge comment { active: false }))
    (ok true)
  )
)

;; Read-only functions

(define-read-only (get-comment (comment-id uint))
  (map-get? project-comments comment-id)
)

(define-read-only (get-project-rating-summary (project-id uint))
  (map-get? project-rating-summary project-id)
)

(define-read-only (get-user-project-rating (project-id uint) (user principal))
  (map-get? project-ratings {project-id: project-id, rater: user})
)

(define-read-only (get-comment-vote (comment-id uint) (voter principal))
  (map-get? comment-votes {comment-id: comment-id, voter: voter})
)

(define-read-only (get-total-comments)
  (var-get comment-count)
)

;; Helper functions

(define-private (is-registered-citizen)
  (let
    (
      (user-role (contract-call? .community get-user-role tx-sender))
    )
    (is-eq user-role u2)  ;; CITIZEN_ROLE from main contract
  )
)

(define-private (is-admin-user)
  (let
    (
      (user-role (contract-call? .community get-user-role tx-sender))
    )
    (is-eq user-role u1)  ;; ADMIN_ROLE from main contract
  )
)

(define-private (project-exists (project-id uint))
  (is-some (contract-call? .community get-project project-id))
)
