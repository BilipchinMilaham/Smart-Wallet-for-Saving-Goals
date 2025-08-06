;; Dynamic Savings Portfolio Manager

;; Error constants
(define-constant ERR-PORTFOLIO-NOT-FOUND (err u300))
(define-constant ERR-TIER-NOT-FOUND (err u301))
(define-constant ERR-INSUFFICIENT-BALANCE (err u302))
(define-constant ERR-INVALID-ALLOCATION (err u303))
(define-constant ERR-PORTFOLIO-LOCKED (err u304))
(define-constant ERR-REBALANCE-TOO-FREQUENT (err u305))
(define-constant ERR-INVALID-RISK-TIER (err u306))
(define-constant ERR-UNAUTHORIZED-ACCESS (err u307))
(define-constant ERR-TIER-ALREADY-EXISTS (err u308))
(define-constant ERR-ALLOCATION-MISMATCH (err u309))

;; Portfolio tiers with different risk/reward profiles
(define-map portfolio-tiers
  { tier-id: uint }
  {
    tier-name: (string-ascii 30),
    base-yield-rate: uint,        ;; Annual yield in basis points
    risk-multiplier: uint,        ;; Risk factor (100 = 1.0x, 150 = 1.5x)
    min-lock-period: uint,        ;; Minimum blocks before withdrawal
    created-by: principal,
    total-allocated: uint,
    tier-description: (string-ascii 100)
  }
)

;; User portfolio configurations
(define-map user-portfolios
  { owner: principal }
  {
    total-value: uint,
    creation-date: uint,
    last-rebalance: uint,
    auto-rebalance-enabled: bool,
    rebalance-threshold: uint,    ;; Percentage deviation to trigger rebalance
    performance-target: uint,     ;; Target annual return in basis points
    risk-tolerance: uint          ;; 1-10 scale
  }
)

;; Individual allocations within portfolios
(define-map portfolio-allocations
  { owner: principal, tier-id: uint }
  {
    allocated-amount: uint,
    target-percentage: uint,      ;; Target allocation percentage (0-100)
    last-yield-claim: uint,
    accrued-yield: uint,
    lock-until-block: uint,
    entry-block: uint
  }
)

;; Portfolio performance tracking
(define-map portfolio-performance
  { owner: principal, period: uint }
  {
    period-start: uint,
    period-end: uint,
    starting-value: uint,
    ending-value: uint,
    total-yield-earned: uint,
    rebalance-count: uint,
    best-performing-tier: uint,
    worst-performing-tier: uint
  }
)

;; Rebalancing rules and triggers
(define-map rebalancing-rules
  { owner: principal }
  {
    auto-rebalance: bool,
    max-deviation: uint,          ;; Max percentage deviation before rebalance
    min-rebalance-interval: uint, ;; Minimum blocks between rebalances
    trigger-conditions: (list 5 uint), ;; List of condition flags
    last-trigger-check: uint
  }
)

;; Market simulation data for dynamic yields
(define-map market-conditions
  { block-period: uint }
  {
    volatility-index: uint,       ;; 0-1000 scale
    base-rate-modifier: uint,     ;; Multiplier for base rates
    risk-premium: uint,           ;; Additional yield for higher risk tiers
    market-trend: (string-ascii 10) ;; "bullish", "bearish", "stable"
  }
)

;; Data variables
(define-data-var tier-counter uint u0)
(define-data-var performance-period-counter uint u0)
(define-data-var global-market-modifier uint u100) ;; Base 100 = 1.0x
(define-data-var rebalance-fee-basis-points uint u25) ;; 0.25% rebalance fee

;; Initialize default tiers
(define-public (initialize-default-tiers)
  (begin
    ;; Conservative tier
    (try! (create-portfolio-tier 
      "Conservative" 
      u200 ;; 2% annual yield
      u75  ;; 0.75x risk multiplier
      u1440 ;; 10 day lock period
      "Low-risk savings tier with stable returns"))
    
    ;; Balanced tier
    (try! (create-portfolio-tier 
      "Balanced" 
      u500 ;; 5% annual yield
      u100 ;; 1.0x risk multiplier
      u2880 ;; 20 day lock period
      "Moderate risk tier balancing growth and stability"))
    
    ;; Growth tier
    (try! (create-portfolio-tier 
      "Growth" 
      u800 ;; 8% annual yield
      u150 ;; 1.5x risk multiplier
      u4320 ;; 30 day lock period
      "Higher risk tier focused on capital appreciation"))
    
    (ok true)
  )
)

;; Create a new portfolio tier
(define-public (create-portfolio-tier 
    (tier-name (string-ascii 30))
    (base-yield-rate uint)
    (risk-multiplier uint)
    (min-lock-period uint)
    (tier-description (string-ascii 100)))
  (let (
    (new-tier-id (+ (var-get tier-counter) u1))
  )
    (asserts! (and (> base-yield-rate u0) (<= base-yield-rate u2000)) ERR-INVALID-RISK-TIER)
    (asserts! (and (>= risk-multiplier u50) (<= risk-multiplier u300)) ERR-INVALID-RISK-TIER)
    
    (var-set tier-counter new-tier-id)
    (ok (map-set portfolio-tiers
      { tier-id: new-tier-id }
      {
        tier-name: tier-name,
        base-yield-rate: base-yield-rate,
        risk-multiplier: risk-multiplier,
        min-lock-period: min-lock-period,
        created-by: tx-sender,
        total-allocated: u0,
        tier-description: tier-description
      }
    ))
  )
)

;; Create user portfolio
(define-public (create-portfolio 
    (risk-tolerance uint)
    (performance-target uint)
    (auto-rebalance bool))
  (let (
    (existing-portfolio (map-get? user-portfolios { owner: tx-sender }))
  )
    (asserts! (is-none existing-portfolio) ERR-PORTFOLIO-NOT-FOUND)
    (asserts! (and (>= risk-tolerance u1) (<= risk-tolerance u10)) ERR-INVALID-RISK-TIER)
    (asserts! (<= performance-target u1500) ERR-INVALID-ALLOCATION) ;; Max 15% target
    
    (ok (map-set user-portfolios
      { owner: tx-sender }
      {
        total-value: u0,
        creation-date: stacks-block-height,
        last-rebalance: stacks-block-height,
        auto-rebalance-enabled: auto-rebalance,
        rebalance-threshold: u10, ;; 10% default threshold
        performance-target: performance-target,
        risk-tolerance: risk-tolerance
      }
    ))
  )
)

;; Allocate funds to a specific tier
(define-public (allocate-to-tier 
    (tier-id uint)
    (amount uint)
    (target-percentage uint))
  (let (
    (portfolio (unwrap! (map-get? user-portfolios { owner: tx-sender }) ERR-PORTFOLIO-NOT-FOUND))
    (tier (unwrap! (map-get? portfolio-tiers { tier-id: tier-id }) ERR-TIER-NOT-FOUND))
    (existing-allocation (map-get? portfolio-allocations { owner: tx-sender, tier-id: tier-id }))
    (current-amount (if (is-some existing-allocation) 
      (get allocated-amount (unwrap-panic existing-allocation)) u0))
    (new-total (+ current-amount amount))
    (lock-period (get min-lock-period tier))
  )
    (asserts! (> amount u0) ERR-INSUFFICIENT-BALANCE)
    (asserts! (<= target-percentage u100) ERR-INVALID-ALLOCATION)
    
    ;; Update portfolio total value
    (map-set user-portfolios
      { owner: tx-sender }
      {
        total-value: (+ (get total-value portfolio) amount),
        creation-date: (get creation-date portfolio),
        last-rebalance: (get last-rebalance portfolio),
        auto-rebalance-enabled: (get auto-rebalance-enabled portfolio),
        rebalance-threshold: (get rebalance-threshold portfolio),
        performance-target: (get performance-target portfolio),
        risk-tolerance: (get risk-tolerance portfolio)
      }
    )
    
    ;; Update tier allocation
    (map-set portfolio-allocations
      { owner: tx-sender, tier-id: tier-id }
      {
        allocated-amount: new-total,
        target-percentage: target-percentage,
        last-yield-claim: stacks-block-height,
        accrued-yield: u0,
        lock-until-block: (+ stacks-block-height lock-period),
        entry-block: stacks-block-height
      }
    )
    
    ;; Update tier total
    (ok (map-set portfolio-tiers
      { tier-id: tier-id }
      {
        tier-name: (get tier-name tier),
        base-yield-rate: (get base-yield-rate tier),
        risk-multiplier: (get risk-multiplier tier),
        min-lock-period: (get min-lock-period tier),
        created-by: (get created-by tier),
        total-allocated: (+ (get total-allocated tier) amount),
        tier-description: (get tier-description tier)
      }
    ))
  )
)

;; Calculate current yield for an allocation
(define-private (calculate-tier-yield 
    (owner principal)
    (tier-id uint))
  (let (
    (allocation (unwrap! (map-get? portfolio-allocations { owner: owner, tier-id: tier-id }) (err u0)))
    (tier (unwrap! (map-get? portfolio-tiers { tier-id: tier-id }) (err u0)))
    (allocated-amount (get allocated-amount allocation))
    (base-rate (get base-yield-rate tier))
    (risk-multiplier (get risk-multiplier tier))
    (blocks-since-claim (- stacks-block-height (get last-yield-claim allocation)))
    (annual-blocks u52560) ;; Approximate blocks per year
    (market-modifier (var-get global-market-modifier))
  )
    (if (> blocks-since-claim u0)
      (let (
        (effective-rate (/ (* base-rate risk-multiplier market-modifier) u10000))
        (period-rate (/ (* effective-rate blocks-since-claim) annual-blocks))
        (yield-amount (/ (* allocated-amount period-rate) u10000))
      )
        (ok yield-amount)
      )
      (ok u0)
    )
  )
)

;; Claim accumulated yield from specific tier
(define-public (claim-tier-yield (tier-id uint))
  (let (
    (allocation (unwrap! (map-get? portfolio-allocations { owner: tx-sender, tier-id: tier-id }) ERR-TIER-NOT-FOUND))
    (calculated-yield (unwrap! (calculate-tier-yield tx-sender tier-id) ERR-TIER-NOT-FOUND))
    (total-yield (+ (get accrued-yield allocation) calculated-yield))
  )
    (asserts! (> total-yield u0) ERR-INSUFFICIENT-BALANCE)
    
    (ok (map-set portfolio-allocations
      { owner: tx-sender, tier-id: tier-id }
      {
        allocated-amount: (+ (get allocated-amount allocation) total-yield),
        target-percentage: (get target-percentage allocation),
        last-yield-claim: stacks-block-height,
        accrued-yield: u0,
        lock-until-block: (get lock-until-block allocation),
        entry-block: (get entry-block allocation)
      }
    ))
  )
)

;; Automated rebalancing function
(define-public (rebalance-portfolio)
  (let (
    (portfolio (unwrap! (map-get? user-portfolios { owner: tx-sender }) ERR-PORTFOLIO-NOT-FOUND))
    (total-value (get total-value portfolio))
    (last-rebalance (get last-rebalance portfolio))
    (min-interval u1440) ;; 10 days minimum between rebalances
  )
    (asserts! (>= (- stacks-block-height last-rebalance) min-interval) ERR-REBALANCE-TOO-FREQUENT)
    (asserts! (get auto-rebalance-enabled portfolio) ERR-PORTFOLIO-LOCKED)
    
    ;; Update last rebalance time
    (map-set user-portfolios
      { owner: tx-sender }
      {
        total-value: (get total-value portfolio),
        creation-date: (get creation-date portfolio),
        last-rebalance: stacks-block-height,
        auto-rebalance-enabled: (get auto-rebalance-enabled portfolio),
        rebalance-threshold: (get rebalance-threshold portfolio),
        performance-target: (get performance-target portfolio),
        risk-tolerance: (get risk-tolerance portfolio)
      }
    )
    
    (ok true)
  )
)

;; Withdraw from tier (respecting lock periods)
(define-public (withdraw-from-tier 
    (tier-id uint)
    (amount uint))
  (let (
    (allocation (unwrap! (map-get? portfolio-allocations { owner: tx-sender, tier-id: tier-id }) ERR-TIER-NOT-FOUND))
    (portfolio (unwrap! (map-get? user-portfolios { owner: tx-sender }) ERR-PORTFOLIO-NOT-FOUND))
    (available-amount (get allocated-amount allocation))
    (lock-until (get lock-until-block allocation))
  )
    (asserts! (>= stacks-block-height lock-until) ERR-PORTFOLIO-LOCKED)
    (asserts! (>= available-amount amount) ERR-INSUFFICIENT-BALANCE)
    
    ;; Update allocation
    (map-set portfolio-allocations
      { owner: tx-sender, tier-id: tier-id }
      {
        allocated-amount: (- available-amount amount),
        target-percentage: (get target-percentage allocation),
        last-yield-claim: (get last-yield-claim allocation),
        accrued-yield: (get accrued-yield allocation),
        lock-until-block: (get lock-until-block allocation),
        entry-block: (get entry-block allocation)
      }
    )
    
    ;; Update portfolio total
    (ok (map-set user-portfolios
      { owner: tx-sender }
      {
        total-value: (- (get total-value portfolio) amount),
        creation-date: (get creation-date portfolio),
        last-rebalance: (get last-rebalance portfolio),
        auto-rebalance-enabled: (get auto-rebalance-enabled portfolio),
        rebalance-threshold: (get rebalance-threshold portfolio),
        performance-target: (get performance-target portfolio),
        risk-tolerance: (get risk-tolerance portfolio)
      }
    ))
  )
)

;; Update market conditions (admin function)
(define-public (update-market-conditions 
    (volatility-index uint)
    (rate-modifier uint)
    (risk-premium uint)
    (trend (string-ascii 10)))
  (let (
    (current-period (/ stacks-block-height u1440)) ;; Daily periods
  )
    (var-set global-market-modifier rate-modifier)
    (ok (map-set market-conditions
      { block-period: current-period }
      {
        volatility-index: volatility-index,
        base-rate-modifier: rate-modifier,
        risk-premium: risk-premium,
        market-trend: trend
      }
    ))
  )
)

;; Read-only functions
(define-read-only (get-portfolio-summary (owner principal))
  (let (
    (portfolio (map-get? user-portfolios { owner: owner }))
  )
    (match portfolio result
      (ok {
        total-value: (get total-value result),
        creation-date: (get creation-date result),
        last-rebalance: (get last-rebalance result),
        auto-rebalance: (get auto-rebalance-enabled result),
        risk-tolerance: (get risk-tolerance result),
        performance-target: (get performance-target result)
      })
      (err ERR-PORTFOLIO-NOT-FOUND)
    )
  )
)

(define-read-only (get-tier-allocation (owner principal) (tier-id uint))
  (map-get? portfolio-allocations { owner: owner, tier-id: tier-id })
)

(define-read-only (get-portfolio-tier (tier-id uint))
  (map-get? portfolio-tiers { tier-id: tier-id })
)

(define-read-only (get-portfolio-performance (owner principal) (period uint))
  (map-get? portfolio-performance { owner: owner, period: period })
)

(define-read-only (calculate-portfolio-value (owner principal))
  (let (
    (portfolio (unwrap! (map-get? user-portfolios { owner: owner }) ERR-PORTFOLIO-NOT-FOUND))
    ;; In a real implementation, this would iterate through all tiers
    ;; For now, return the stored total value
  )
    (ok (get total-value portfolio))
  )
)

(define-read-only (get-market-conditions (period uint))
  (map-get? market-conditions { block-period: period })
)

(define-read-only (check-rebalance-needed (owner principal))
  (let (
    (portfolio (unwrap! (map-get? user-portfolios { owner: owner }) ERR-PORTFOLIO-NOT-FOUND))
    (threshold (get rebalance-threshold portfolio))
    (last-rebalance (get last-rebalance portfolio))
    (min-interval u1440)
  )
    (ok {
      needs-rebalance: (and 
        (get auto-rebalance-enabled portfolio)
        (>= (- stacks-block-height last-rebalance) min-interval)
      ),
      time-until-eligible: (if (>= (- stacks-block-height last-rebalance) min-interval)
        u0
        (- min-interval (- stacks-block-height last-rebalance))),
      current-threshold: threshold
    })
  )
)

;; Get all available tiers
(define-read-only (get-available-tiers)
  (ok (var-get tier-counter))
)
