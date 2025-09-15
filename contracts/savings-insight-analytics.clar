;; Savings Insight & Analytics Dashboard
;; Personalized financial insights and automated savings recommendations
;; Analyzes user behavior to optimize savings strategies

;; Error constants
(define-constant ERR-USER-NOT-FOUND (err u400))
(define-constant ERR-INSUFFICIENT-DATA (err u401))
(define-constant ERR-INVALID-TIMEFRAME (err u402))
(define-constant ERR-UNAUTHORIZED-ACCESS (err u403))

;; Data variables
(define-data-var insight-counter uint u0)
(define-data-var analytics-active bool true)

;; User behavior tracking maps
(define-map user-behavior-analytics
  { user: principal }
  {
    total-deposits: uint,
    average-deposit-size: uint,
    deposit-frequency: uint,
    last-deposit-date: uint,
    savings-consistency-score: uint,
    behavioral-pattern: (string-ascii 20)
  }
)

(define-map savings-velocity-metrics
  { user: principal }
  {
    weekly-savings-rate: uint,
    monthly-projection: uint,
    goal-completion-prediction: uint,
    savings-acceleration: uint,
    trend-direction: (string-ascii 10)
  }
)

(define-map personalized-insights
  { user: principal, insight-id: uint }
  {
    insight-type: (string-ascii 30),
    message: (string-ascii 200),
    confidence-score: uint,
    generated-at: uint,
    actionable: bool
  }
)

(define-map financial-health-scores
  { user: principal }
  {
    overall-score: uint,
    savings-discipline: uint,
    goal-achievement-rate: uint,
    consistency-rating: uint,
    improvement-potential: uint,
    last-calculated: uint
  }
)

(define-map spending-pattern-analysis
  { user: principal, period: uint }
  {
    period-start: uint,
    period-end: uint,
    total-deposits: uint,
    deposit-count: uint,
    largest-deposit: uint,
    smallest-deposit: uint,
    savings-streaks: uint,
    missed-opportunities: uint
  }
)

;; Public functions for analytics data collection

;; Record user deposit for behavioral analysis
(define-public (record-deposit-behavior (user principal) (amount uint))
  (let (
    (current-behavior (default-to 
      { total-deposits: u0, average-deposit-size: u0, deposit-frequency: u0, 
        last-deposit-date: u0, savings-consistency-score: u50, behavioral-pattern: "new-user" }
      (map-get? user-behavior-analytics { user: user })))
    (new-total-deposits (+ (get total-deposits current-behavior) u1))
    (new-average-size (/ (+ (* (get average-deposit-size current-behavior) (get total-deposits current-behavior)) amount) 
                        new-total-deposits))
    (blocks-since-last (- stacks-block-height (get last-deposit-date current-behavior)))
    (new-frequency (if (> (get total-deposits current-behavior) u0) 
                     (/ blocks-since-last (get total-deposits current-behavior)) u144))
  )
    ;; Update behavior analytics
    (map-set user-behavior-analytics { user: user } {
      total-deposits: new-total-deposits,
      average-deposit-size: new-average-size,
      deposit-frequency: new-frequency,
      last-deposit-date: stacks-block-height,
      savings-consistency-score: (calculate-consistency-score user),
      behavioral-pattern: (determine-behavior-pattern new-frequency amount)
    })
    
    ;; Update savings velocity
    (update-savings-velocity user amount)
    (ok true)
  )
)

;; Generate personalized savings insights
(define-public (generate-insights (user principal))
  (let (
    (behavior (unwrap! (map-get? user-behavior-analytics { user: user }) ERR-USER-NOT-FOUND))
    (velocity (unwrap! (map-get? savings-velocity-metrics { user: user }) ERR-INSUFFICIENT-DATA))
    (new-insight-id (+ (var-get insight-counter) u1))
  )
    (var-set insight-counter new-insight-id)
    
    ;; Generate multiple insights based on user data
    (let (
      (insight-type (determine-insight-type behavior velocity))
      (insight-message (generate-insight-message insight-type behavior))
      (confidence (calculate-insight-confidence behavior))
    )
      (map-set personalized-insights { user: user, insight-id: new-insight-id } {
        insight-type: insight-type,
        message: insight-message,
        confidence-score: confidence,
        generated-at: stacks-block-height,
        actionable: true
      })
      
      (ok new-insight-id)
    )
  )
)

;; Calculate comprehensive financial health score
(define-public (calculate-financial-health (user principal))
  (let (
    (behavior (unwrap! (map-get? user-behavior-analytics { user: user }) ERR-USER-NOT-FOUND))
    (velocity (unwrap! (map-get? savings-velocity-metrics { user: user }) ERR-USER-NOT-FOUND))
  )
    (let (
      (discipline-score (get savings-consistency-score behavior))
      (velocity-score (if (< (/ (get weekly-savings-rate velocity) u10) u100)
        (/ (get weekly-savings-rate velocity) u10) u100))
      (frequency-score (if (< (/ u1440 (get deposit-frequency behavior)) u100)
        (/ u1440 (get deposit-frequency behavior)) u100))
      (overall-score (/ (+ discipline-score velocity-score frequency-score) u3))
    )
      (map-set financial-health-scores { user: user } {
        overall-score: overall-score,
        savings-discipline: discipline-score,
        goal-achievement-rate: velocity-score,
        consistency-rating: frequency-score,
        improvement-potential: (- u100 overall-score),
        last-calculated: stacks-block-height
      })
      
      (ok overall-score)
    )
  )
)

;; Analyze spending patterns for a period
(define-public (analyze-spending-pattern (user principal) (period-blocks uint))
  (let (
    (period-id (/ stacks-block-height period-blocks))
    (period-start (- stacks-block-height period-blocks))
    (behavior (unwrap! (map-get? user-behavior-analytics { user: user }) ERR-USER-NOT-FOUND))
  )
    (asserts! (> period-blocks u0) ERR-INVALID-TIMEFRAME)
    
    (map-set spending-pattern-analysis { user: user, period: period-id } {
      period-start: period-start,
      period-end: stacks-block-height,
      total-deposits: (get total-deposits behavior),
      deposit-count: (get total-deposits behavior),
      largest-deposit: (get average-deposit-size behavior),
      smallest-deposit: (/ (get average-deposit-size behavior) u2),
      savings-streaks: (get savings-consistency-score behavior),
      missed-opportunities: (if (> (- u10 (get savings-consistency-score behavior)) u0)
        (- u10 (get savings-consistency-score behavior)) u0)
    })
    
    (ok period-id)
  )
)

;; Private helper functions

;; Update savings velocity metrics
(define-private (update-savings-velocity (user principal) (amount uint))
  (let (
    (current-velocity (default-to 
      { weekly-savings-rate: u0, monthly-projection: u0, goal-completion-prediction: u0, 
        savings-acceleration: u100, trend-direction: "stable" }
      (map-get? savings-velocity-metrics { user: user })))
    (blocks-per-week u1008)
    (weekly-rate (/ (* amount blocks-per-week) u144))
    (monthly-projection (* weekly-rate u4))
  )
    (map-set savings-velocity-metrics { user: user } {
      weekly-savings-rate: weekly-rate,
      monthly-projection: monthly-projection,
      goal-completion-prediction: (+ stacks-block-height u4320),
      savings-acceleration: u100,
      trend-direction: "positive"
    })
  )
)

;; Calculate user consistency score
(define-private (calculate-consistency-score (user principal))
  (let (
    (behavior (unwrap! (map-get? user-behavior-analytics { user: user }) u50))
  )
    (if (> (get total-deposits behavior) u0)
(if (< (+ u50 (* (get total-deposits behavior) u5)) u100)
        (+ u50 (* (get total-deposits behavior) u5))
        u100)
      u50
    )
  )
)

;; Determine user behavioral pattern
(define-private (determine-behavior-pattern (frequency uint) (amount uint))
  (if (< frequency u144)
    "frequent-saver"
    (if (< frequency u1008)
      "weekly-saver"
      "occasional-saver"
    )
  )
)

;; Determine insight type based on user data
(define-private (determine-insight-type (behavior {total-deposits: uint, average-deposit-size: uint, deposit-frequency: uint, last-deposit-date: uint, savings-consistency-score: uint, behavioral-pattern: (string-ascii 20)}) (velocity {weekly-savings-rate: uint, monthly-projection: uint, goal-completion-prediction: uint, savings-acceleration: uint, trend-direction: (string-ascii 10)}))
  (if (< (get savings-consistency-score behavior) u30)
    "consistency-improvement"
    (if (< (get weekly-savings-rate velocity) u50)
      "increase-savings-rate"
      "optimization-tips"
    )
  )
)

;; Generate insight message based on type
(define-private (generate-insight-message (insight-type (string-ascii 30)) (behavior {total-deposits: uint, average-deposit-size: uint, deposit-frequency: uint, last-deposit-date: uint, savings-consistency-score: uint, behavioral-pattern: (string-ascii 20)}))
  (if (is-eq insight-type "consistency-improvement")
    "Try saving smaller amounts more frequently to build a consistent habit"
    (if (is-eq insight-type "increase-savings-rate")
      "Consider increasing your weekly savings rate to reach goals faster"
      "You're doing great! Consider setting up auto-save for steady progress"
    )
  )
)

;; Calculate insight confidence level
(define-private (calculate-insight-confidence (behavior {total-deposits: uint, average-deposit-size: uint, deposit-frequency: uint, last-deposit-date: uint, savings-consistency-score: uint, behavioral-pattern: (string-ascii 20)}))
  (if (> (get total-deposits behavior) u10)
    u85
    (if (> (get total-deposits behavior) u5)
      u70
      u50
    )
  )
)

;; Read-only functions for accessing insights

;; Get user behavior analytics
(define-read-only (get-user-behavior (user principal))
  (map-get? user-behavior-analytics { user: user })
)

;; Get savings velocity metrics
(define-read-only (get-savings-velocity (user principal))
  (map-get? savings-velocity-metrics { user: user })
)

;; Get personalized insight
(define-read-only (get-insight (user principal) (insight-id uint))
  (map-get? personalized-insights { user: user, insight-id: insight-id })
)

;; Get financial health score
(define-read-only (get-financial-health (user principal))
  (map-get? financial-health-scores { user: user })
)

;; Get spending pattern analysis
(define-read-only (get-spending-patterns (user principal) (period uint))
  (map-get? spending-pattern-analysis { user: user, period: period })
)

;; Calculate goal optimization recommendation
(define-read-only (recommend-goal-optimization (user principal))
  (let (
    (behavior (unwrap! (map-get? user-behavior-analytics { user: user }) ERR-USER-NOT-FOUND))
    (velocity (unwrap! (map-get? savings-velocity-metrics { user: user }) ERR-USER-NOT-FOUND))
  )
    (ok {
      recommended-goal-size: (* (get weekly-savings-rate velocity) u12),
      suggested-frequency: (get deposit-frequency behavior),
      optimization-score: (get savings-consistency-score behavior),
      confidence-level: u80
    })
  )
)

;; Get savings efficiency rating
(define-read-only (calculate-savings-efficiency (user principal))
  (let (
    (behavior (default-to 
      { total-deposits: u0, average-deposit-size: u0, deposit-frequency: u0, 
        last-deposit-date: u0, savings-consistency-score: u50, behavioral-pattern: "new-user" }
      (map-get? user-behavior-analytics { user: user })))
    (health (map-get? financial-health-scores { user: user }))
  )
    (if (is-some health)
      (let ((health-data (unwrap-panic health)))
        {
          efficiency-rating: (get overall-score health-data),
          improvement-areas: (get improvement-potential health-data),
          current-discipline: (get savings-discipline health-data)
        }
      )
      {
        efficiency-rating: u50,
        improvement-areas: u50,
        current-discipline: u50
      }
    )
  )
)
