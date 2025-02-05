;; Smart Wallet for Saving Goals

;; Constants
(define-constant ERR-GOAL-NOT-REACHED (err u100))
(define-constant ERR-INSUFFICIENT-FUNDS (err u101))

;; Data Maps

(define-public (deposit (amount uint) (goal-id uint))
  (let (
    (current-goal (unwrap! (map-get? savings-goals { owner: tx-sender, goal-id: goal-id }) (err u102)))
    (new-amount (+ (get current-amount current-goal) amount))
  )
    (if (>= amount u0)
      (ok (map-set savings-goals
        { owner: tx-sender, goal-id: goal-id }
        {
          target-amount: (get target-amount current-goal),
          current-amount: new-amount,
          goal-name: (get goal-name current-goal)
        }
      ))
      ERR-INSUFFICIENT-FUNDS
    )
  )
)

(define-public (withdraw (amount uint) (goal-id uint))
  (let (
    (current-goal (unwrap! (map-get? savings-goals { owner: tx-sender, goal-id: goal-id  }) (err u102)))
  )
    (if (>= (get current-amount current-goal) (get target-amount current-goal))
      (ok (map-set savings-goals
        { owner: tx-sender, goal-id: goal-id }
        {
          target-amount: (get target-amount current-goal),
          current-amount: (- (get current-amount current-goal) amount),
          goal-name: (get goal-name current-goal)
        }
      ))
      ERR-GOAL-NOT-REACHED
    )
  )
)

;; Read Only Functions
(define-read-only (get-goal (owner principal) (goal-id uint))
  (map-get? savings-goals { owner: owner, goal-id: goal-id   })
)



;; Add a goal ID to track multiple goals
(define-map savings-goals
  { owner: principal, goal-id: uint }
  {
    target-amount: uint,
    current-amount: uint,
    goal-name: (string-ascii 50)
  }
)

(define-data-var goal-counter uint u0)

(define-public (create-goal (target uint) (name (string-ascii 50)))
  (let
    ((new-goal-id (+ (var-get goal-counter) u1)))
    (var-set goal-counter new-goal-id)
    (ok (map-set savings-goals
      { owner: tx-sender, goal-id: new-goal-id }
      {
        target-amount: target,
        current-amount: u0,
        goal-name: name
      }
    ))
  )
)



;; Add deadline to goals map
(define-map savings-goals-with-deadline
  { owner: principal, goal-id: uint }
  {
    target-amount: uint,
    current-amount: uint,
    goal-name: (string-ascii 50),
    deadline: uint,
    is-active: bool
  }
)

(define-public (create-goal-with-deadline (target uint) (name (string-ascii 50)) (deadline-blocks uint))
  (let
    ((new-goal-id (+ (var-get goal-counter) u1)))
    (var-set goal-counter new-goal-id)
    (ok (map-set savings-goals-with-deadline
      { owner: tx-sender, goal-id: new-goal-id }
      {
        target-amount: target,
        current-amount: u0,
        goal-name: name,
        deadline: (+ stacks-block-height deadline-blocks),
        is-active: true
      }
    ))
  )
)


(define-read-only (get-goal-progress (owner principal) (goal-id uint))
  (let (
    (goal (unwrap! (map-get? savings-goals { owner: owner, goal-id: goal-id }) (err u102)))
  )
    (ok {
      percentage: (/ (* (get current-amount goal) u100) (get target-amount goal)),
      remaining: (- (get target-amount goal) (get current-amount goal))
    })
  )
)
