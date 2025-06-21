;; Smart Wallet for Saving Goals

;; Constants
(define-constant ERR-GOAL-NOT-REACHED (err u100))
(define-constant ERR-INSUFFICIENT-FUNDS (err u101))

;; NFT Definition
(define-non-fungible-token achievement-nft uint)

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


(define-map sub-goals
  { parent-goal-id: uint, sub-goal-id: uint }
  {
    target-amount: uint,
    current-amount: uint,
    name: (string-ascii 50),
    completed: bool
  }
)

(define-data-var sub-goal-counter uint u0)

(define-public (create-sub-goal 
    (parent-goal-id uint) 
    (target uint) 
    (name (string-ascii 50)))
  (let (
    (new-sub-id (+ (var-get sub-goal-counter) u1))
    (parent-goal (unwrap! (map-get? savings-goals 
      { owner: tx-sender, goal-id: parent-goal-id }) (err u107)))
  )
    (var-set sub-goal-counter new-sub-id)
    (ok (map-set sub-goals
      { parent-goal-id: parent-goal-id, sub-goal-id: new-sub-id }
      {
        target-amount: target,
        current-amount: u0,
        name: name,
        completed: false
      }
    ))
  )
)

(define-read-only (get-sub-goals (parent-goal-id uint))
  (map-get? sub-goals { parent-goal-id: parent-goal-id, sub-goal-id: (var-get sub-goal-counter) })
)


(define-map goal-analytics
  { owner: principal }
  {
    total-goals-created: uint,
    goals-completed: uint,
    total-saved: uint,
    average-completion-time: uint,
    last-activity: uint
  }
)

(define-public (update-analytics (goal-completed bool) (amount-saved uint))
  (let (
    (current-stats (default-to 
      { 
        total-goals-created: u0, 
        goals-completed: u0, 
        total-saved: u0,
        average-completion-time: u0,
        last-activity: u0 
      }
      (map-get? goal-analytics { owner: tx-sender })))
  )
    (ok (map-set goal-analytics
      { owner: tx-sender }
      {
        total-goals-created: (+ (get total-goals-created current-stats) u1),
        goals-completed: (+ (get goals-completed current-stats) 
          (if goal-completed u1 u0)),
        total-saved: (+ (get total-saved current-stats) amount-saved),
        average-completion-time: stacks-block-height,
        last-activity: stacks-block-height
      }
    ))
  )
)

(define-read-only (get-user-analytics (owner principal))
  (map-get? goal-analytics { owner: owner })
)


(define-constant ERR-INVALID-SPLIT-AMOUNT (err u110))
(define-constant ERR-UNAUTHORIZED (err u111))

(define-map goal-splits
  { original-goal-id: uint, split-id: uint }
  {
    amount: uint,
    recipient: principal,
    status: (string-ascii 10)
  }
)

(define-data-var split-counter uint u0)

(define-public (split-and-transfer-goal 
    (goal-id uint)
    (split-amount uint)
    (recipient principal))
  (let (
    (current-goal (unwrap! (map-get? savings-goals 
      { owner: tx-sender, goal-id: goal-id }) (err u102)))
    (new-split-id (+ (var-get split-counter) u1))
  )
    (asserts! (>= (get current-amount current-goal) split-amount)
      ERR-INVALID-SPLIT-AMOUNT)
    
    (var-set split-counter new-split-id)
    
    (unwrap! (create-goal split-amount "Split Goal Transfer") (err u112))
    
    (ok (map-set goal-splits
      { original-goal-id: goal-id, split-id: new-split-id }
      {
        amount: split-amount,
        recipient: recipient,
        status: "pending"
      }))
  ))

(define-public (accept-goal-split (original-goal-id uint) (split-id uint))
  (let (
    (split-details (unwrap! (map-get? goal-splits
      { original-goal-id: original-goal-id, split-id: split-id })
      ERR-UNAUTHORIZED))
  )
    (asserts! (is-eq tx-sender (get recipient split-details))
      ERR-UNAUTHORIZED)
    
    (ok (map-set goal-splits
      { original-goal-id: original-goal-id, split-id: split-id }
      {
        amount: (get amount split-details),
        recipient: tx-sender,
        status: "accepted"
      }))
  ))



(define-map nft-metadata
  { token-id: uint }
  {
    goal-name: (string-ascii 50),
    achievement-date: uint,
    amount-saved: uint
  }
)

(define-data-var nft-counter uint u0)

(define-public (mint-achievement-nft (goal-id uint))
  (let (
    (goal (unwrap! (map-get? savings-goals
      { owner: tx-sender, goal-id: goal-id }) (err u102)))
    (new-token-id (+ (var-get nft-counter) u1))
  )
    (asserts! (>= (get current-amount goal) (get target-amount goal))
      ERR-GOAL-NOT-REACHED)
    
    (var-set nft-counter new-token-id)
    
    ;; (try! (nft-mint achievement-nft new-token-id tx-sender))
    
    (ok (map-set nft-metadata
      { token-id: new-token-id }
      {
        goal-name: (get goal-name goal),
        achievement-date: stacks-block-height,
        amount-saved: (get target-amount goal)
      }))
  ))

(define-read-only (get-achievement-metadata (token-id uint))
  (map-get? nft-metadata { token-id: token-id }))



(define-constant ERR-INVALID-INTEREST-RATE (err u120))
(define-constant ERR-COMPOUNDING-NOT-DUE (err u121))

(define-map goal-interest-settings
  { owner: principal, goal-id: uint }
  {
    annual-rate: uint,
    compound-frequency: uint,
    last-compound: uint,
    total-interest-earned: uint,
    is-enabled: bool
  }
)

(define-constant BLOCKS-PER-YEAR u52560)
(define-constant MAX-INTEREST-RATE u1000)
(define-constant RATE-PRECISION u10000)

(define-public (set-goal-interest-rate 
    (goal-id uint) 
    (annual-rate-basis-points uint)
    (compound-frequency-blocks uint))
  (let (
    (goal (unwrap! (map-get? savings-goals 
      { owner: tx-sender, goal-id: goal-id }) (err u102)))
  )
    (asserts! (<= annual-rate-basis-points MAX-INTEREST-RATE) ERR-INVALID-INTEREST-RATE)
    (ok (map-set goal-interest-settings
      { owner: tx-sender, goal-id: goal-id }
      {
        annual-rate: annual-rate-basis-points,
        compound-frequency: compound-frequency-blocks,
        last-compound: stacks-block-height,
        total-interest-earned: u0,
        is-enabled: true
      }
    ))
  )
)

(define-public (compound-interest (goal-id uint))
  (let (
    (goal (unwrap! (map-get? savings-goals 
      { owner: tx-sender, goal-id: goal-id }) (err u102)))
    (interest-settings (unwrap! (map-get? goal-interest-settings
      { owner: tx-sender, goal-id: goal-id }) (err u122)))
    (blocks-since-compound (- stacks-block-height (get last-compound interest-settings)))
    (current-amount (get current-amount goal))
  )
    (asserts! (get is-enabled interest-settings) (err u123))
    (asserts! (>= blocks-since-compound (get compound-frequency interest-settings)) 
      ERR-COMPOUNDING-NOT-DUE)
    
    (let (
      (periods-elapsed (/ blocks-since-compound (get compound-frequency interest-settings)))
      (period-rate (/ (get annual-rate interest-settings) 
        (/ BLOCKS-PER-YEAR (get compound-frequency interest-settings))))
      (interest-amount (/ (* current-amount period-rate periods-elapsed) RATE-PRECISION))
      (new-amount (+ current-amount interest-amount))
    )
      (map-set savings-goals
        { owner: tx-sender, goal-id: goal-id }
        {
          target-amount: (get target-amount goal),
          current-amount: new-amount,
          goal-name: (get goal-name goal)
        }
      )
      (ok (map-set goal-interest-settings
        { owner: tx-sender, goal-id: goal-id }
        {
          annual-rate: (get annual-rate interest-settings),
          compound-frequency: (get compound-frequency interest-settings),
          last-compound: stacks-block-height,
          total-interest-earned: (+ (get total-interest-earned interest-settings) interest-amount),
          is-enabled: (get is-enabled interest-settings)
        }
      ))
    )
  )
)

(define-public (toggle-interest-compounding (goal-id uint) (enabled bool))
  (let (
    (interest-settings (unwrap! (map-get? goal-interest-settings
      { owner: tx-sender, goal-id: goal-id }) (err u122)))
  )
    (ok (map-set goal-interest-settings
      { owner: tx-sender, goal-id: goal-id }
      {
        annual-rate: (get annual-rate interest-settings),
        compound-frequency: (get compound-frequency interest-settings),
        last-compound: (get last-compound interest-settings),
        total-interest-earned: (get total-interest-earned interest-settings),
        is-enabled: enabled
      }
    ))
  )
)

(define-read-only (calculate-future-value 
    (goal-id uint) 
    (owner principal)
    (future-blocks uint))
  (let (
    (goal (unwrap! (map-get? savings-goals 
      { owner: owner, goal-id: goal-id }) (err u102)))
    (interest-settings (unwrap! (map-get? goal-interest-settings
      { owner: owner, goal-id: goal-id }) (err u122)))
    (current-amount (get current-amount goal))
    (annual-rate (get annual-rate interest-settings))
    (compound-frequency (get compound-frequency interest-settings))
  )
    (if (get is-enabled interest-settings)
      (let (
        (periods (/ future-blocks compound-frequency))
        (period-rate (/ annual-rate (/ BLOCKS-PER-YEAR compound-frequency)))
        (compound-multiplier (+ RATE-PRECISION period-rate))
        (future-value (/ (* current-amount (pow compound-multiplier periods)) 
          (pow RATE-PRECISION periods)))
      )
        (ok {
          current-value: current-amount,
          future-value: future-value,
          interest-earned: (- future-value current-amount),
          periods: periods
        })
      )
      (ok {
        current-value: current-amount,
        future-value: current-amount,
        interest-earned: u0,
        periods: u0
      })
    )
  )
)

(define-read-only (get-interest-settings (owner principal) (goal-id uint))
  (map-get? goal-interest-settings { owner: owner, goal-id: goal-id })
)

(define-read-only (check-compound-eligibility (owner principal) (goal-id uint))
  (let (
    (interest-settings (unwrap! (map-get? goal-interest-settings
      { owner: owner, goal-id: goal-id }) (err u122)))
    (blocks-since-compound (- stacks-block-height (get last-compound interest-settings)))
  )
    (ok {
      is-due: (>= blocks-since-compound (get compound-frequency interest-settings)),
      blocks-remaining: (if (>= blocks-since-compound (get compound-frequency interest-settings))
        u0
        (- (get compound-frequency interest-settings) blocks-since-compound)),
      next-compound-block: (+ (get last-compound interest-settings) 
        (get compound-frequency interest-settings))
    })
  )
)