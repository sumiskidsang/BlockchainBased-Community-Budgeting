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

(define-constant ERR_DELEGATION_NOT_FOUND (err u110))
(define-constant ERR_ALREADY_DELEGATED (err u111))
(define-constant ERR_SELF_DELEGATION (err u112))
(define-constant ERR_DELEGATION_REVOKED (err u113))
(define-constant ERR_INSUFFICIENT_REPUTATION (err u114))

(define-map user-delegations principal
  {
    delegate: principal,
    active: bool,
    voting-power: uint
  }
)

(define-map delegate-vote-counts principal uint)

(define-public (delegate-voting-power (delegate principal) (voting-power uint))
  (let
    (
      (delegator-rep (get-user-reputation-score tx-sender))
      (max-delegation (/ delegator-rep u10))
    )
    (asserts! (>= delegator-rep u30) ERR_INSUFFICIENT_REPUTATION)
    (asserts! (not (is-eq tx-sender delegate)) ERR_SELF_DELEGATION)
    (asserts! (is-citizen) ERR_UNAUTHORIZED)
    (asserts! (is-registered) ERR_UNAUTHORIZED)
    (asserts! (<= voting-power max-delegation) ERR_INVALID_AMOUNT)
    (asserts! (> voting-power u0) ERR_INVALID_AMOUNT)
    (asserts! (is-none (map-get? user-delegations tx-sender)) ERR_ALREADY_DELEGATED)
    
    (map-set user-delegations tx-sender {
      delegate: delegate,
      active: true,
      voting-power: voting-power
    })
    (map-set delegate-vote-counts delegate 
      (+ (default-to u0 (map-get? delegate-vote-counts delegate)) voting-power))
    (ok true)
  )
)

(define-public (revoke-delegation)
  (let
    (
      (delegation (unwrap! (map-get? user-delegations tx-sender) ERR_DELEGATION_NOT_FOUND))
      (delegate (get delegate delegation))
      (voting-power (get voting-power delegation))
    )
    (asserts! (get active delegation) ERR_DELEGATION_REVOKED)
    
    (map-set user-delegations tx-sender (merge delegation {active: false}))
    (map-set delegate-vote-counts delegate 
      (- (default-to u0 (map-get? delegate-vote-counts delegate)) voting-power))
    (ok true)
  )
)

(define-public (vote-with-delegation (project-id uint) (delegator principal))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
      (delegation (unwrap! (map-get? user-delegations delegator) ERR_DELEGATION_NOT_FOUND))
      (vote-key {project-id: project-id, voter: delegator})
      (voting-power (get voting-power delegation))
    )
    (asserts! (is-citizen) ERR_UNAUTHORIZED)
    (asserts! (var-get voting-open) ERR_VOTING_CLOSED)
    (asserts! (get active project) ERR_PROJECT_INACTIVE)
    (asserts! (get active delegation) ERR_DELEGATION_REVOKED)
    (asserts! (is-eq tx-sender (get delegate delegation)) ERR_UNAUTHORIZED)
    (asserts! (is-none (map-get? project-votes vote-key)) ERR_ALREADY_VOTED)
    
    (map-set project-votes vote-key {voted: true})
    (map-set projects project-id (merge project {votes: (+ (get votes project) voting-power)}))
    (ok true)
  )
)

(define-read-only (get-user-delegation (user principal))
  (map-get? user-delegations user)
)

(define-read-only (get-delegate-vote-count (delegate principal))
  (default-to u0 (map-get? delegate-vote-counts delegate))
)

(define-read-only (get-max-delegation-power (user principal))
  (/ (get-user-reputation-score user) u10)
)

(define-constant ERR_ESCROW_NOT_FOUND (err u115))
(define-constant ERR_ESCROW_ALREADY_EXISTS (err u116))
(define-constant ERR_ESCROW_LOCKED (err u117))
(define-constant ERR_ESCROW_RELEASED (err u118))
(define-constant ERR_INSUFFICIENT_ESCROW (err u119))

(define-map project-escrow uint
  {
    total-amount: uint,
    released-amount: uint,
    locked: bool,
    release-deadline: uint,
    creator: principal,
    escrow-active: bool
  }
)

(define-public (create-escrow (project-id uint) (deadline-blocks uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
      (requested-amount (get requested-amount project))
      (creator (get creator project))
      (current-block stacks-block-height)
    )
    (asserts! (is-admin) ERR_UNAUTHORIZED)
    (asserts! (get funded project) ERR_PROJECT_INACTIVE)
    (asserts! (is-none (map-get? project-escrow project-id)) ERR_ESCROW_ALREADY_EXISTS)
    (asserts! (> deadline-blocks u0) ERR_INVALID_AMOUNT)
    
    (map-set project-escrow project-id {
      total-amount: requested-amount,
      released-amount: u0,
      locked: true,
      release-deadline: (+ current-block deadline-blocks),
      creator: creator,
      escrow-active: true
    })
    (ok true)
  )
)

(define-public (release-escrow-funds (project-id uint) (amount uint))
  (let
    (
      (escrow (unwrap! (map-get? project-escrow project-id) ERR_ESCROW_NOT_FOUND))
      (total-amount (get total-amount escrow))
      (released-amount (get released-amount escrow))
      (available-amount (- total-amount released-amount))
    )
    (asserts! (is-admin) ERR_UNAUTHORIZED)
    (asserts! (get escrow-active escrow) ERR_ESCROW_RELEASED)
    (asserts! (get locked escrow) ERR_ESCROW_LOCKED)
    (asserts! (<= amount available-amount) ERR_INSUFFICIENT_ESCROW)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    
    (map-set project-escrow project-id 
      (merge escrow { released-amount: (+ released-amount amount) })
    )
    (ok amount)
  )
)

(define-public (unlock-escrow (project-id uint))
  (let
    (
      (escrow (unwrap! (map-get? project-escrow project-id) ERR_ESCROW_NOT_FOUND))
      (current-block stacks-block-height)
      (deadline (get release-deadline escrow))
    )
    (asserts! (or (is-admin) (is-eq tx-sender (get creator escrow))) ERR_UNAUTHORIZED)
    (asserts! (get escrow-active escrow) ERR_ESCROW_RELEASED)
    (asserts! (>= current-block deadline) ERR_VOTING_CLOSED)
    
    (map-set project-escrow project-id 
      (merge escrow { locked: false })
    )
    (ok true)
  )
)

(define-public (close-escrow (project-id uint))
  (let
    (
      (escrow (unwrap! (map-get? project-escrow project-id) ERR_ESCROW_NOT_FOUND))
      (total-amount (get total-amount escrow))
      (released-amount (get released-amount escrow))
      (remaining-amount (- total-amount released-amount))
    )
    (asserts! (is-admin) ERR_UNAUTHORIZED)
    (asserts! (get escrow-active escrow) ERR_ESCROW_RELEASED)
    
    (var-set total-budget (+ (var-get total-budget) remaining-amount))
    (map-set project-escrow project-id 
      (merge escrow { escrow-active: false })
    )
    (ok remaining-amount)
  )
)

(define-read-only (get-escrow-details (project-id uint))
  (map-get? project-escrow project-id)
)

(define-read-only (get-escrow-remaining (project-id uint))
  (match (map-get? project-escrow project-id)
    escrow (- (get total-amount escrow) (get released-amount escrow))
    u0
  )
)

(define-read-only (is-escrow-unlocked (project-id uint))
  (match (map-get? project-escrow project-id)
    escrow (not (get locked escrow))
    false
  )
)

;; Budget Cycle Management System
(define-constant ERR_CYCLE_NOT_FOUND (err u120))
(define-constant ERR_CYCLE_ALREADY_EXISTS (err u121))
(define-constant ERR_CYCLE_ACTIVE (err u122))
(define-constant ERR_CYCLE_INACTIVE (err u123))
(define-constant ERR_INVALID_CYCLE_DURATION (err u124))
(define-constant ERR_CYCLE_NOT_ENDED (err u125))

;; Cycle status constants
(define-constant CYCLE_STATUS_PLANNING u1)
(define-constant CYCLE_STATUS_ACTIVE u2)
(define-constant CYCLE_STATUS_VOTING u3)
(define-constant CYCLE_STATUS_FUNDING u4)
(define-constant CYCLE_STATUS_COMPLETED u5)

;; Current active cycle ID
(define-data-var current-cycle-id uint u0)
(define-data-var total-cycles uint u0)

;; Budget cycle data structure
(define-map budget-cycles uint
  {
    name: (string-ascii 50),
    description: (string-ascii 200),
    total-budget: uint,
    allocated-budget: uint,
    start-block: uint,
    end-block: uint,
    voting-start: uint,
    voting-end: uint,
    status: uint,
    projects-count: uint,
    total-votes: uint,
    created-by: principal,
    created-at: uint
  }
)

;; Track projects by cycle
(define-map cycle-projects
  {
    cycle-id: uint,
    project-id: uint
  }
  {
    included: bool
  }
)

;; Track user participation per cycle
(define-map cycle-participation
  {
    cycle-id: uint,
    user: principal
  }
  {
    proposals-submitted: uint,
    votes-cast: uint,
    projects-funded: uint,
    participation-score: uint
  }
)

;; Cycle statistics for analytics
(define-map cycle-statistics uint
  {
    total-participants: uint,
    budget-utilization: uint,
    average-project-votes: uint,
    success-rate: uint
  }
)

;; Create a new budget cycle
(define-public (create-budget-cycle 
  (name (string-ascii 50)) 
  (description (string-ascii 200)) 
  (budget uint) 
  (duration-blocks uint)
  (voting-duration uint))
  (let
    (
      (cycle-id (+ (var-get total-cycles) u1))
      (current-block stacks-block-height)
      (end-block (+ current-block duration-blocks))
      (voting-start (+ current-block (/ duration-blocks u4))) ;; Voting starts at 25% of cycle
      (voting-end (+ voting-start voting-duration))
    )
    (asserts! (is-admin) ERR_UNAUTHORIZED)
    (asserts! (> budget u0) ERR_INVALID_AMOUNT)
    (asserts! (> duration-blocks u100) ERR_INVALID_CYCLE_DURATION) ;; Minimum 100 blocks
    (asserts! (< voting-duration duration-blocks) ERR_INVALID_CYCLE_DURATION)
    (asserts! (< voting-end end-block) ERR_INVALID_CYCLE_DURATION)
    
    ;; Close current cycle if exists
    (if (> (var-get current-cycle-id) u0)
      (try! (close-current-cycle))
      true
    )
    
    ;; Create new cycle
    (map-set budget-cycles cycle-id {
      name: name,
      description: description,
      total-budget: budget,
      allocated-budget: u0,
      start-block: current-block,
      end-block: end-block,
      voting-start: voting-start,
      voting-end: voting-end,
      status: CYCLE_STATUS_PLANNING,
      projects-count: u0,
      total-votes: u0,
      created-by: tx-sender,
      created-at: current-block
    })
    
    ;; Initialize cycle statistics
    (map-set cycle-statistics cycle-id {
      total-participants: u0,
      budget-utilization: u0,
      average-project-votes: u0,
      success-rate: u0
    })
    
    ;; Update global state
    (var-set total-cycles cycle-id)
    (var-set current-cycle-id cycle-id)
    (var-set total-budget budget)
    (var-set voting-open false) ;; Initially closed until cycle starts
    
    (ok cycle-id)
  )
)

;; Activate the current cycle (transition from planning to active)
(define-public (activate-cycle (cycle-id uint))
  (let
    (
      (cycle (unwrap! (map-get? budget-cycles cycle-id) ERR_CYCLE_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (is-admin) ERR_UNAUTHORIZED)
    (asserts! (is-eq cycle-id (var-get current-cycle-id)) ERR_CYCLE_INACTIVE)
    (asserts! (is-eq (get status cycle) CYCLE_STATUS_PLANNING) ERR_CYCLE_ACTIVE)
    (asserts! (>= current-block (get start-block cycle)) ERR_VOTING_CLOSED)
    
    (map-set budget-cycles cycle-id 
      (merge cycle { status: CYCLE_STATUS_ACTIVE })
    )
    (ok true)
  )
)

;; Start voting phase for current cycle
(define-public (start-cycle-voting (cycle-id uint))
  (let
    (
      (cycle (unwrap! (map-get? budget-cycles cycle-id) ERR_CYCLE_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (is-admin) ERR_UNAUTHORIZED)
    (asserts! (is-eq cycle-id (var-get current-cycle-id)) ERR_CYCLE_INACTIVE)
    (asserts! (is-eq (get status cycle) CYCLE_STATUS_ACTIVE) ERR_CYCLE_INACTIVE)
    (asserts! (>= current-block (get voting-start cycle)) ERR_VOTING_CLOSED)
    (asserts! (< current-block (get voting-end cycle)) ERR_VOTING_CLOSED)
    
    (map-set budget-cycles cycle-id 
      (merge cycle { status: CYCLE_STATUS_VOTING })
    )
    (var-set voting-open true)
    (ok true)
  )
)

;; End voting phase and transition to funding
(define-public (end-cycle-voting (cycle-id uint))
  (let
    (
      (cycle (unwrap! (map-get? budget-cycles cycle-id) ERR_CYCLE_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (is-admin) ERR_UNAUTHORIZED)
    (asserts! (is-eq cycle-id (var-get current-cycle-id)) ERR_CYCLE_INACTIVE)
    (asserts! (is-eq (get status cycle) CYCLE_STATUS_VOTING) ERR_CYCLE_INACTIVE)
    (asserts! (>= current-block (get voting-end cycle)) ERR_VOTING_OPEN)
    
    (map-set budget-cycles cycle-id 
      (merge cycle { status: CYCLE_STATUS_FUNDING })
    )
    (var-set voting-open false)
    (ok true)
  )
)

;; Close current cycle and finalize statistics
(define-public (close-current-cycle)
  (let
    (
      (cycle-id (var-get current-cycle-id))
      (cycle (unwrap! (map-get? budget-cycles cycle-id) ERR_CYCLE_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (is-admin) ERR_UNAUTHORIZED)
    (asserts! (> cycle-id u0) ERR_CYCLE_NOT_FOUND)
    (asserts! (>= current-block (get end-block cycle)) ERR_CYCLE_NOT_ENDED)
    
    ;; Calculate final statistics
    (let
      (
        (cycle-total-budget (get total-budget cycle))
        (cycle-allocated-budget (get allocated-budget cycle))
        (budget-utilization (if (> cycle-total-budget u0) (/ (* cycle-allocated-budget u100) cycle-total-budget) u0))
        (cycle-projects-count (get projects-count cycle))
        (cycle-total-votes (get total-votes cycle))
        (avg-votes (if (> cycle-projects-count u0) (/ cycle-total-votes cycle-projects-count) u0))
      )
      (map-set cycle-statistics cycle-id 
        (merge (unwrap-panic (map-get? cycle-statistics cycle-id)) {
          budget-utilization: budget-utilization,
          average-project-votes: avg-votes,
          success-rate: (if (> cycle-projects-count u0) (/ (* cycle-allocated-budget u100) cycle-total-budget) u0)
        })
      )
    )
    
    ;; Mark cycle as completed
    (map-set budget-cycles cycle-id 
      (merge cycle { status: CYCLE_STATUS_COMPLETED })
    )
    
    (ok true)
  )
)

;; Add project to current cycle
(define-public (add-project-to-cycle (project-id uint))
  (let
    (
      (cycle-id (var-get current-cycle-id))
      (cycle (unwrap! (map-get? budget-cycles cycle-id) ERR_CYCLE_NOT_FOUND))
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
    )
    (asserts! (> cycle-id u0) ERR_CYCLE_NOT_FOUND)
    (asserts! (or (is-eq (get status cycle) CYCLE_STATUS_ACTIVE) 
                 (is-eq (get status cycle) CYCLE_STATUS_VOTING)) ERR_CYCLE_INACTIVE)
    
    ;; Track project in cycle
    (map-set cycle-projects 
      { cycle-id: cycle-id, project-id: project-id }
      { included: true }
    )
    
    ;; Update cycle project count
    (map-set budget-cycles cycle-id 
      (merge cycle { projects-count: (+ (get projects-count cycle) u1) })
    )
    
    ;; Update user participation
    (unwrap-panic (update-user-participation cycle-id (get creator project) u1 u0 u0))
    (ok true)
  )
)

;; Update user participation in cycle
(define-private (update-user-participation 
  (cycle-id uint) 
  (user principal) 
  (proposals uint) 
  (votes uint) 
  (funded uint))
  (let
    (
      (current-participation (default-to 
        { proposals-submitted: u0, votes-cast: u0, projects-funded: u0, participation-score: u0 }
        (map-get? cycle-participation { cycle-id: cycle-id, user: user })
      ))
    )
    (map-set cycle-participation 
      { cycle-id: cycle-id, user: user }
      {
        proposals-submitted: (+ (get proposals-submitted current-participation) proposals),
        votes-cast: (+ (get votes-cast current-participation) votes),
        projects-funded: (+ (get projects-funded current-participation) funded),
        participation-score: (+ (get participation-score current-participation) proposals votes funded)
      }
    )
    (ok true)
  )
)

;; Read-only functions for cycle management
(define-read-only (get-current-cycle-id)
  (var-get current-cycle-id)
)

(define-read-only (get-cycle-details (cycle-id uint))
  (map-get? budget-cycles cycle-id)
)

(define-read-only (get-cycle-statistics (cycle-id uint))
  (map-get? cycle-statistics cycle-id)
)

(define-read-only (get-user-cycle-participation (cycle-id uint) (user principal))
  (map-get? cycle-participation { cycle-id: cycle-id, user: user })
)

(define-read-only (is-project-in-cycle (cycle-id uint) (project-id uint))
  (default-to false 
    (get included (map-get? cycle-projects { cycle-id: cycle-id, project-id: project-id }))
  )
)

(define-read-only (get-cycle-status (cycle-id uint))
  (match (map-get? budget-cycles cycle-id)
    cycle (get status cycle)
    u0
  )
)

(define-read-only (is-cycle-voting-active (cycle-id uint))
  (let
    (
      (cycle (map-get? budget-cycles cycle-id))
      (current-block stacks-block-height)
    )
    (match cycle
      cycle-data 
        (and 
          (is-eq (get status cycle-data) CYCLE_STATUS_VOTING)
          (>= current-block (get voting-start cycle-data))
          (< current-block (get voting-end cycle-data))
        )
      false
    )
  )
)

(define-read-only (get-total-cycles)
  (var-get total-cycles)
)



