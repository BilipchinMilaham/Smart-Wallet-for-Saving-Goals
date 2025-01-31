;; Smart Wallet for Saving Goals

;; Constants
(define-constant ERR-GOAL-NOT-REACHED (err u100))
(define-constant ERR-INSUFFICIENT-FUNDS (err u101))

;; Data Maps
(define-map savings-goals
  { owner: principal }
  {
    target-amount: uint,
    current-amount: uint,
    goal-name: (string-ascii 50)
  }
)

;; Public Functions
(define-public (create-goal (target uint) (name (string-ascii 50)))
  (ok (map-set savings-goals
    { owner: tx-sender }
    {
      target-amount: target,
      current-amount: u0,
      goal-name: name
    }
  ))
)

(define-public (deposit (amount uint))
  (let (
    (current-goal (unwrap! (map-get? savings-goals { owner: tx-sender }) (err u102)))
    (new-amount (+ (get current-amount current-goal) amount))
  )
    (if (>= amount u0)
      (ok (map-set savings-goals
        { owner: tx-sender }
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

(define-public (withdraw (amount uint))
  (let (
    (current-goal (unwrap! (map-get? savings-goals { owner: tx-sender }) (err u102)))
  )
    (if (>= (get current-amount current-goal) (get target-amount current-goal))
      (ok (map-set savings-goals
        { owner: tx-sender }
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
(define-read-only (get-goal (owner principal))
  (map-get? savings-goals { owner: owner })
)
