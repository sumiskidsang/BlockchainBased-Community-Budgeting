(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_ALREADY_VOTED (err u101))
(define-constant ERR_PROJECT_NOT_FOUND (err u102))
(define-constant ERR_PROJECT_ALREADY_EXISTS (err u103))
(define-constant ERR_INSUFFICIENT_FUNDS (err u104))
(define-constant ERR_PROJECT_INACTIVE (err u105))
(define-constant ERR_VOTING_CLOSED (err u106))
(define-constant ERR_VOTING_OPEN (err u107))
(define-constant ERR_ALREADY_FUNDED (err u108))
(define-constant ERR_INVALID_AMOUNT (err u109))

(define-constant ADMIN_ROLE u1)
(define-constant CITIZEN_ROLE u2)

(define-data-var total-budget uint u0)
(define-data-var voting-open bool true)

(define-map users principal
  {
    role: uint,
    registered: bool
  }
)

(define-map projects uint
  {
    name: (string-ascii 50),
    description: (string-ascii 500),
    requested-amount: uint,
    votes: uint,
    active: bool,
    funded: bool,
    creator: principal
  }
)

(define-map project-votes
  {
    project-id: uint,
    voter: principal
  }
  {
    voted: bool
  }
)

(define-map project-id-to-index uint uint)
(define-data-var project-count uint u0)

(define-public (register-user (role uint))
  (begin
    (asserts! (or (is-eq tx-sender contract-caller) (is-admin)) ERR_UNAUTHORIZED)
    (ok (map-set users tx-sender {role: role, registered: true}))
  )
)

(define-public (register-citizen)
  (register-user CITIZEN_ROLE)
)

(define-public (register-admin)
  (begin
    (asserts! (is-admin) ERR_UNAUTHORIZED)
    (register-user ADMIN_ROLE)
  )
)

(define-public (set-budget (amount uint))
  (begin
    (asserts! (is-admin) ERR_UNAUTHORIZED)
    (var-set total-budget amount)
    (ok amount)
  )
)

(define-public (toggle-voting)
  (begin
    (asserts! (is-admin) ERR_UNAUTHORIZED)
    (var-set voting-open (not (var-get voting-open)))
    (ok (var-get voting-open))
  )
)

(define-public (propose-project (name (string-ascii 50)) (description (string-ascii 500)) (amount uint))
  (let
    (
      (project-id (var-get project-count))
    )
    (asserts! (is-registered) ERR_UNAUTHORIZED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (var-get voting-open) ERR_VOTING_CLOSED)
    
    (var-set project-count (+ project-id u1))
    (map-set projects project-id {
      name: name,
      description: description,
      requested-amount: amount,
      votes: u0,
      active: true,
      funded: false,
      creator: tx-sender
    })
    (map-set project-id-to-index project-id project-id)
    (ok project-id)
  )
)

(define-public (vote-for-project (project-id uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
      (vote-key {project-id: project-id, voter: tx-sender})
    )
    (asserts! (is-citizen) ERR_UNAUTHORIZED)
    (asserts! (var-get voting-open) ERR_VOTING_CLOSED)
    (asserts! (get active project) ERR_PROJECT_INACTIVE)
    (asserts! (is-none (map-get? project-votes vote-key)) ERR_ALREADY_VOTED)
    
    (map-set project-votes vote-key {voted: true})
    (map-set projects project-id (merge project {votes: (+ (get votes project) u1)}))
    (ok true)
  )
)

(define-public (cancel-project (project-id uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
    )
    (asserts! (or (is-admin) (is-eq tx-sender (get creator project))) ERR_UNAUTHORIZED)
    (asserts! (not (get funded project)) ERR_ALREADY_FUNDED)
    
    (map-set projects project-id (merge project {active: false}))
    (ok true)
  )
)

(define-public (fund-project (project-id uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
      (budget (var-get total-budget))
    )
    (asserts! (is-admin) ERR_UNAUTHORIZED)
    (asserts! (not (var-get voting-open)) ERR_VOTING_OPEN)
    (asserts! (get active project) ERR_PROJECT_INACTIVE)
    (asserts! (not (get funded project)) ERR_ALREADY_FUNDED)
    (asserts! (<= (get requested-amount project) budget) ERR_INSUFFICIENT_FUNDS)
    
    (var-set total-budget (- budget (get requested-amount project)))
    (map-set projects project-id (merge project {funded: true}))
    (ok true)
  )
)

(define-public (reactivate-project (project-id uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
    )
    (asserts! (is-admin) ERR_UNAUTHORIZED)
    (asserts! (var-get voting-open) ERR_VOTING_CLOSED)
    (asserts! (not (get funded project)) ERR_ALREADY_FUNDED)
    
    (map-set projects project-id (merge project {active: true}))
    (ok true)
  )
)

(define-read-only (get-project (project-id uint))
  (map-get? projects project-id)
)

(define-read-only (get-project-vote (project-id uint) (voter principal))
  (map-get? project-votes {project-id: project-id, voter: voter})
)

(define-read-only (get-user-role (user principal))
  (default-to u0 (get role (map-get? users user)))
)

(define-read-only (get-budget)
  (var-get total-budget)
)

(define-read-only (is-voting-open)
  (var-get voting-open)
)

(define-read-only (get-project-count)
  (var-get project-count)
)

(define-private (is-admin)
  (is-eq (get-user-role tx-sender) ADMIN_ROLE)
)

(define-private (is-citizen)
  (is-eq (get-user-role tx-sender) CITIZEN_ROLE)
)

(define-private (is-registered)
  (default-to false (get registered (map-get? users tx-sender)))
)

(define-map project-milestones 
  { project-id: uint }
  {
    total-milestones: uint,
    completed-milestones: uint,
    amount-per-milestone: uint
  }
)

(define-public (add-project-milestones (project-id uint) (milestone-count uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
      (amount-per-mile (/ (get requested-amount project) milestone-count))
    )
    (asserts! (is-admin) ERR_UNAUTHORIZED)
    (asserts! (> milestone-count u0) ERR_INVALID_AMOUNT)
    (map-set project-milestones
      { project-id: project-id }
      {
        total-milestones: milestone-count,
        completed-milestones: u0,
        amount-per-milestone: amount-per-mile
      }
    )
    (ok true)
  )
)

(define-public (complete-milestone (project-id uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
      (milestones (unwrap! (map-get? project-milestones { project-id: project-id }) ERR_PROJECT_NOT_FOUND))
    )
    (asserts! (is-admin) ERR_UNAUTHORIZED)
    (asserts! (< (get completed-milestones milestones) (get total-milestones milestones)) ERR_INVALID_AMOUNT)
    
    (map-set project-milestones
      { project-id: project-id }
      (merge milestones { completed-milestones: (+ (get completed-milestones milestones) u1) })
    )
    (ok true)
  )
)


(define-constant CATEGORY_INFRASTRUCTURE u1)
(define-constant CATEGORY_EDUCATION u2)
(define-constant CATEGORY_ENVIRONMENT u3)
(define-constant CATEGORY_TECHNOLOGY u4)

(define-map project-categories uint uint)

(define-public (set-project-category (project-id uint) (category uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
    )
    (asserts! (is-admin) ERR_UNAUTHORIZED)
    (asserts! (or 
      (is-eq category CATEGORY_INFRASTRUCTURE)
      (is-eq category CATEGORY_EDUCATION)
      (is-eq category CATEGORY_ENVIRONMENT)
      (is-eq category CATEGORY_TECHNOLOGY)
    ) ERR_INVALID_AMOUNT)
    
    (map-set project-categories project-id category)
    (ok true)
  )
)

;; (define-read-only (get-projects-by-category (category uint))
;;   (filter (lambda (project) (is-eq (get-category (get project-id project)) category)) projects)
;; )

(define-constant REPUTATION_PROPOSAL_BONUS u10)
(define-constant REPUTATION_VOTE_BONUS u2)
(define-constant REPUTATION_COMPLETION_BONUS u25)
(define-constant REPUTATION_CANCELLATION_PENALTY u15)

(define-map user-reputation principal
  {
    total-score: uint,
    proposals-created: uint,
    votes-cast: uint,
    projects-completed: uint,
    projects-cancelled: uint
  }
)

(define-map reputation-history
  {
    user: principal,
    action-id: uint
  }
  {
    action-type: (string-ascii 20),
    points-change: int,
    timestamp: uint,
    related-project: (optional uint)
  }
)

(define-data-var reputation-action-count uint u0)

(define-private (get-user-reputation-data (user principal))
  (default-to 
    {
      total-score: u0,
      proposals-created: u0,
      votes-cast: u0,
      projects-completed: u0,
      projects-cancelled: u0
    }
    (map-get? user-reputation user)
  )
)

(define-private (award-reputation (user principal) (points uint) (action-type (string-ascii 20)) (project-id (optional uint)))
  (let
    (
      (current-rep (get-user-reputation-data user))
      (action-id (var-get reputation-action-count))
    )
    (map-set user-reputation user
      (merge current-rep { total-score: (+ (get total-score current-rep) points) })
    )
    (map-set reputation-history
      { user: user, action-id: action-id }
      {
        action-type: action-type,
        points-change: (to-int points),
        timestamp: stacks-block-height,
        related-project: project-id
      }
    )
    (var-set reputation-action-count (+ action-id u1))
    (ok { code: u0 })
  )
)

(define-private (deduct-reputation (user principal) (points uint) (action-type (string-ascii 20)) (project-id (optional uint)))
  (let
    (
      (current-rep (get-user-reputation-data user))
      (action-id (var-get reputation-action-count))
      (current-score (get total-score current-rep))
      (new-score (if (>= current-score points) (- current-score points) u0))
    )
    (map-set user-reputation user
      (merge current-rep { total-score: new-score })
    )
    (map-set reputation-history
      { user: user, action-id: action-id }
      {
        action-type: action-type,
        points-change: (- (to-int points)),
        timestamp: stacks-block-height,
        related-project: project-id
      }
    )
    (var-set reputation-action-count (+ action-id u1))
    (ok true)
  )
)

(define-public (propose-project-with-reputation (name (string-ascii 50)) (description (string-ascii 500)) (amount uint))
  (let
    (
      (project-id (var-get project-count))
      (current-rep (get-user-reputation-data tx-sender))
    )
    (asserts! (is-registered) ERR_UNAUTHORIZED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (var-get voting-open) ERR_VOTING_CLOSED)
    
    (var-set project-count (+ project-id u1))
    (map-set projects project-id {
      name: name,
      description: description,
      requested-amount: amount,
      votes: u0,
      active: true,
      funded: false,
      creator: tx-sender
    })
    (map-set project-id-to-index project-id project-id)
    (map-set user-reputation tx-sender
      (merge current-rep { proposals-created: (+ (get proposals-created current-rep) u1) })
    )
    ;; (try! (award-reputation tx-sender REPUTATION_PROPOSAL_BONUS "proposal" (some project-id)))
    (ok project-id)
  )
)

(define-public (vote-for-project-with-reputation (project-id uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
      (vote-key {project-id: project-id, voter: tx-sender})
      (current-rep (get-user-reputation-data tx-sender))
    )
    (asserts! (is-citizen) ERR_UNAUTHORIZED)
    (asserts! (var-get voting-open) ERR_VOTING_CLOSED)
    (asserts! (get active project) ERR_PROJECT_INACTIVE)
    (asserts! (is-none (map-get? project-votes vote-key)) ERR_ALREADY_VOTED)
    
    (map-set project-votes vote-key {voted: true})
    (map-set projects project-id (merge project {votes: (+ (get votes project) u1)}))
    (map-set user-reputation tx-sender
      (merge current-rep { votes-cast: (+ (get votes-cast current-rep) u1) })
    )
    ;; (try! (award-reputation tx-sender REPUTATION_VOTE_BONUS "vote" (some project-id)))
    (ok true)
  )
)

(define-public (mark-project-completed (project-id uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
      (creator (get creator project))
      (current-rep (get-user-reputation-data creator))
    )
    (asserts! (is-admin) ERR_UNAUTHORIZED)
    (asserts! (get funded project) ERR_PROJECT_INACTIVE)
    
    (map-set user-reputation creator
      (merge current-rep { projects-completed: (+ (get projects-completed current-rep) u1) })
    )
    ;; (try! (award-reputation creator REPUTATION_COMPLETION_BONUS "completion" (some project-id)))
    (ok true)
  )
)

(define-public (cancel-project-with-reputation (project-id uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
      (creator (get creator project))
      (current-rep (get-user-reputation-data creator))
    )
    (asserts! (or (is-admin) (is-eq tx-sender creator)) ERR_UNAUTHORIZED)
    (asserts! (not (get funded project)) ERR_ALREADY_FUNDED)
    
    (map-set projects project-id (merge project {active: false}))
    (map-set user-reputation creator
      (merge current-rep { projects-cancelled: (+ (get projects-cancelled current-rep) u1) })
    )
    ;; (try! (deduct-reputation creator REPUTATION_CANCELLATION_PENALTY "cancellation" (some project-id)))
    (ok true)
  )
)

(define-read-only (get-user-reputation (user principal))
  (get-user-reputation-data user)
)

(define-read-only (get-user-reputation-score (user principal))
  (get total-score (get-user-reputation-data user))
)

(define-read-only (get-reputation-history (user principal) (action-id uint))
  (map-get? reputation-history { user: user, action-id: action-id })
)

(define-read-only (is-trusted-user (user principal))
  (>= (get-user-reputation-score user) u50)
)

(define-read-only (get-reputation-multiplier (user principal))
  (let
    (
      (score (get-user-reputation-score user))
    )
    (if (>= score u100) u3
      (if (>= score u50) u2 u1)
    )
  )
)