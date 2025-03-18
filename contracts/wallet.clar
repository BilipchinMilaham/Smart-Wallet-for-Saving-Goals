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




(define-constant ERR-INVALID-PASSWORD (err u103))
(define-data-var emergency-password uint u0)

(define-public (set-emergency-password (password uint))
  (ok (var-set emergency-password password))
)

(define-public (emergency-withdraw (amount uint) (password uint) (goal-id uint))
  (let (
    (current-goal (unwrap! (map-get? savings-goals { owner: tx-sender, goal-id: goal-id }) (err u102)))
  )
    (if (and 
      (<= amount (get current-amount current-goal))
      (is-eq password (var-get emergency-password)))
      (ok (map-set savings-goals
        { owner: tx-sender, goal-id: goal-id }
        {
          target-amount: (get target-amount current-goal),
          current-amount: (- (get current-amount current-goal) amount),
          goal-name: (get goal-name current-goal)
        }
      ))
      ERR-INVALID-PASSWORD
    )
  )
)



(define-map goal-categories
  { category-id: uint }
  { category-name: (string-ascii 20) }
)

(define-public (add-category (id uint) (name (string-ascii 20)))
  (ok (map-set goal-categories
    { category-id: id }
    { category-name: name }
  ))
)

(define-read-only (get-category (id uint))
  (map-get? goal-categories { category-id: id })
)



(define-map milestone-rewards
  { owner: principal, milestone: uint }
  { reward-amount: uint }
)

(define-public (set-milestone-reward (milestone-percentage uint) (reward uint))
  (ok (map-set milestone-rewards
    { owner: tx-sender, milestone: milestone-percentage }
    { reward-amount: reward }
  ))
)

(define-read-only (check-milestone-reward (owner principal) (milestone uint))
  (map-get? milestone-rewards { owner: owner, milestone: milestone })
)




(define-map shared-goals
  { goal-owner: principal, shared-with: principal }
  { can-view: bool, can-contribute: bool }
)

(define-public (share-goal (share-with principal) (allow-contributions bool))
  (ok (map-set shared-goals
    { goal-owner: tx-sender, shared-with: share-with }
    { can-view: true, can-contribute: allow-contributions }
  ))
)

(define-read-only (get-shared-permissions (owner principal) (viewer principal))
  (map-get? shared-goals { goal-owner: owner, shared-with: viewer })
)


;; Define notification types
(define-constant NOTIFY-25-PERCENT u25)
(define-constant NOTIFY-50-PERCENT u50)
(define-constant NOTIFY-75-PERCENT u75)
(define-constant NOTIFY-100-PERCENT u100)

;; Store notification status
(define-map goal-notifications
  { owner: principal, goal-id: uint }
  { 
    last-notification: uint,
    notifications-enabled: bool
  }
)

(define-public (toggle-notifications (goal-id uint) (enabled bool))
  (ok (map-set goal-notifications
    { owner: tx-sender, goal-id: goal-id }
    { last-notification: u0, notifications-enabled: enabled }
  ))
)

(define-read-only (check-notification-milestone (owner principal) (goal-id uint))
  (let (
    (goal (unwrap! (map-get? savings-goals { owner: owner, goal-id: goal-id }) (err u102)))
    (notifications (default-to { last-notification: u0, notifications-enabled: false }
      (map-get? goal-notifications { owner: owner, goal-id: goal-id })))
    (progress (/ (* (get current-amount goal) u100) (get target-amount goal)))
  )
    (if (get notifications-enabled notifications)
      (ok {
        should-notify: (> progress (get last-notification notifications)),
        progress: progress
      })
      (ok { should-notify: false, progress: progress })
    )
  )
)


(define-map achievement-streaks
  { owner: principal }
  { 
    current-streak: uint,
    longest-streak: uint,
    last-completion: uint
  }
)

(define-constant STREAK-EXPIRY-BLOCKS u144) ;; About 1 day in blocks

(define-public (update-achievement-streak (completed bool))
  (let (
    (current-stats (default-to { current-streak: u0, longest-streak: u0, last-completion: u0 }
      (map-get? achievement-streaks { owner: tx-sender })))
    (new-streak (if (and 
                     completed 
                     (< (- stacks-block-height (get last-completion current-stats)) STREAK-EXPIRY-BLOCKS))
                  (+ (get current-streak current-stats) u1)
                  (if completed u1 u0)))
    (new-longest (if (> new-streak (get longest-streak current-stats))
                    new-streak
                    (get longest-streak current-stats)))
  )
    (ok (map-set achievement-streaks
      { owner: tx-sender }
      {
        current-streak: new-streak,
        longest-streak: new-longest,
        last-completion: (if completed stacks-block-height (get last-completion current-stats))
      }
    ))
  )
)

(define-map goal-templates
  { template-id: uint }
  {
    name: (string-ascii 50),
    suggested-amount: uint,
    category: (string-ascii 20),
    description: (string-ascii 100)
  }
)

(define-data-var template-counter uint u0)

(define-public (create-template 
    (name (string-ascii 50)) 
    (amount uint)
    (category (string-ascii 20))
    (description (string-ascii 100)))
  (let ((new-id (+ (var-get template-counter) u1)))
    (var-set template-counter new-id)
    (ok (map-set goal-templates
      { template-id: new-id }
      {
        name: name,
        suggested-amount: amount,
        category: category,
        description: description
      }
    ))
  )
)

(define-public (start-from-template (template-id uint))
  (let ((template (unwrap! (map-get? goal-templates { template-id: template-id }) (err u104))))
    (create-goal (get suggested-amount template) (get name template))
  )
)


(define-map savings-groups
  { group-id: uint }
  {
    name: (string-ascii 50),
    target-amount: uint,
    current-amount: uint,
    member-count: uint,
    creator: principal
  }
)

(define-map group-members
  { group-id: uint, member: principal }
  { joined-at: uint, contribution: uint }
)

(define-data-var group-counter uint u0)

(define-public (create-savings-group (name (string-ascii 50)) (target uint))
  (let ((new-id (+ (var-get group-counter) u1)))
    (var-set group-counter new-id)
    (map-set savings-groups
      { group-id: new-id }
      {
        name: name,
        target-amount: target,
        current-amount: u0,
        member-count: u1,
        creator: tx-sender
      }
    )
    (ok (map-set group-members
      { group-id: new-id, member: tx-sender }
      { joined-at: stacks-block-height, contribution: u0 }
    ))
  )
)

(define-public (join-savings-group (group-id uint))
  (let ((group (unwrap! (map-get? savings-groups { group-id: group-id }) (err u105))))
    (ok (map-set group-members
      { group-id: group-id, member: tx-sender }
      { joined-at: stacks-block-height, contribution: u0 }
    ))
  )
)

(define-public (contribute-to-group (group-id uint) (amount uint))
  (let (
    (group (unwrap! (map-get? savings-groups { group-id: group-id }) (err u105)))
    (member (unwrap! (map-get? group-members { group-id: group-id, member: tx-sender }) (err u106)))
    (new-amount (+ (get contribution member) amount))
  )
    (if (>= amount u0)
      (ok (map-set group-members
        { group-id: group-id, member: tx-sender }
        { joined-at: (get joined-at member), contribution: new-amount }
      ))
      ERR-INSUFFICIENT-FUNDS
    )
  )
)

(define-map goal-tags
  { goal-id: uint, tag: (string-ascii 20) }
  { added-at: uint }
)

(define-map user-tags
  { owner: principal }
  { tags-list: (list 20 (string-ascii 20)) }
)

(define-public (add-goal-tag (goal-id uint) (tag (string-ascii 20)))
  (ok (map-set goal-tags
    { goal-id: goal-id, tag: tag }
    { added-at: stacks-block-height }
  ))
)

(define-public (remove-goal-tag (goal-id uint) (tag (string-ascii 20)))
  (ok (map-delete goal-tags { goal-id: goal-id, tag: tag }))
)

(define-map auto-save-rules
  { owner: principal, goal-id: uint }
  {
    amount: uint,
    frequency: uint, ;; in blocks
    last-save: uint,
    is-active: bool
  }
)

(define-constant DAILY-BLOCKS u144)
(define-constant WEEKLY-BLOCKS u1008)
(define-constant MONTHLY-BLOCKS u4320)

(define-public (set-auto-save-rule 
    (goal-id uint) 
    (amount uint)
    (frequency uint))
  (ok (map-set auto-save-rules
    { owner: tx-sender, goal-id: goal-id }
    {
      amount: amount,
      frequency: frequency,
      last-save: stacks-block-height,
      is-active: true
    }
  ))
)

(define-public (toggle-auto-save (goal-id uint) (enabled bool))
  (let ((rule (unwrap! (map-get? auto-save-rules 
          { owner: tx-sender, goal-id: goal-id }) 
          (err u106))))
    (ok (map-set auto-save-rules
      { owner: tx-sender, goal-id: goal-id }
      {
        amount: (get amount rule),
        frequency: (get frequency rule),
        last-save: (get last-save rule),
        is-active: enabled
      }
    ))
  )
)