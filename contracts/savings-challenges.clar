;; Smart Savings Challenge System

(define-constant ERR-CHALLENGE-NOT-FOUND (err u200))
(define-constant ERR-CHALLENGE-EXPIRED (err u201))
(define-constant ERR-CHALLENGE-NOT-ACTIVE (err u202))
(define-constant ERR-ALREADY-PARTICIPATING (err u203))
(define-constant ERR-INVALID-CHALLENGE-PERIOD (err u204))
(define-constant ERR-INSUFFICIENT-REWARD-POOL (err u205))
(define-constant ERR-CHALLENGE-NOT-ENDED (err u206))
(define-constant ERR-UNAUTHORIZED-CHALLENGE (err u207))

(define-map challenges
  { challenge-id: uint }
  {
    title: (string-ascii 50),
    description: (string-ascii 100),
    target-amount: uint,
    reward-pool: uint,
    start-block: uint,
    end-block: uint,
    max-participants: uint,
    current-participants: uint,
    creator: principal,
    is-active: bool,
    challenge-type: (string-ascii 20)
  }
)

(define-map challenge-participants
  { challenge-id: uint, participant: principal }
  {
    joined-at: uint,
    current-savings: uint,
    last-deposit: uint,
    rank: uint,
    completed: bool
  }
)

(define-map challenge-leaderboard
  { challenge-id: uint, rank: uint }
  {
    participant: principal,
    amount: uint,
    completion-time: uint
  }
)

(define-map user-challenge-stats
  { user: principal }
  {
    challenges-joined: uint,
    challenges-completed: uint,
    total-winnings: uint,
    best-rank: uint,
    current-streak: uint
  }
)

(define-data-var challenge-counter uint u0)

(define-public (create-challenge 
    (title (string-ascii 50))
    (description (string-ascii 100))
    (target-amount uint)
    (reward-pool uint)
    (duration-blocks uint)
    (max-participants uint)
    (challenge-type (string-ascii 20)))
  (let (
    (new-challenge-id (+ (var-get challenge-counter) u1))
    (start-block stacks-block-height)
    (end-block (+ stacks-block-height duration-blocks))
  )
    (asserts! (> duration-blocks u0) ERR-INVALID-CHALLENGE-PERIOD)
    (asserts! (> target-amount u0) ERR-INVALID-CHALLENGE-PERIOD)
    (asserts! (>= reward-pool u0) ERR-INSUFFICIENT-REWARD-POOL)
    
    (var-set challenge-counter new-challenge-id)
    
    (ok (map-set challenges
      { challenge-id: new-challenge-id }
      {
        title: title,
        description: description,
        target-amount: target-amount,
        reward-pool: reward-pool,
        start-block: start-block,
        end-block: end-block,
        max-participants: max-participants,
        current-participants: u0,
        creator: tx-sender,
        is-active: true,
        challenge-type: challenge-type
      }
    ))
  )
)

(define-public (join-challenge (challenge-id uint))
  (let (
    (challenge (unwrap! (map-get? challenges { challenge-id: challenge-id }) ERR-CHALLENGE-NOT-FOUND))
    (existing-participation (map-get? challenge-participants 
      { challenge-id: challenge-id, participant: tx-sender }))
  )
    (asserts! (is-none existing-participation) ERR-ALREADY-PARTICIPATING)
    (asserts! (get is-active challenge) ERR-CHALLENGE-NOT-ACTIVE)
    (asserts! (< stacks-block-height (get end-block challenge)) ERR-CHALLENGE-EXPIRED)
    (asserts! (< (get current-participants challenge) (get max-participants challenge)) ERR-CHALLENGE-NOT-ACTIVE)
    
    (map-set challenges
      { challenge-id: challenge-id }
      {
        title: (get title challenge),
        description: (get description challenge),
        target-amount: (get target-amount challenge),
        reward-pool: (get reward-pool challenge),
        start-block: (get start-block challenge),
        end-block: (get end-block challenge),
        max-participants: (get max-participants challenge),
        current-participants: (+ (get current-participants challenge) u1),
        creator: (get creator challenge),
        is-active: (get is-active challenge),
        challenge-type: (get challenge-type challenge)
      }
    )
    
    (ok (map-set challenge-participants
      { challenge-id: challenge-id, participant: tx-sender }
      {
        joined-at: stacks-block-height,
        current-savings: u0,
        last-deposit: u0,
        rank: u0,
        completed: false
      }
    ))
  )
)

(define-public (record-challenge-deposit (challenge-id uint) (amount uint))
  (let (
    (challenge-result (map-get? challenges { challenge-id: challenge-id }))
  )
    (match challenge-result challenge
      (let (
        (participant-result (map-get? challenge-participants { challenge-id: challenge-id, participant: tx-sender }))
      )
        (match participant-result participant
          (let (
            (new-savings (+ (get current-savings participant) amount))
          )
            (asserts! (get is-active challenge) ERR-CHALLENGE-NOT-ACTIVE)
            (asserts! (< stacks-block-height (get end-block challenge)) ERR-CHALLENGE-EXPIRED)
            (asserts! (> amount u0) ERR-INSUFFICIENT-REWARD-POOL)
            
            (let (
              (challenge-completed (>= new-savings (get target-amount challenge)))
              (completion-time (if challenge-completed stacks-block-height u0))
            )
              (map-set challenge-participants
                { challenge-id: challenge-id, participant: tx-sender }
                {
                  joined-at: (get joined-at participant),
                  current-savings: new-savings,
                  last-deposit: stacks-block-height,
                  rank: (get rank participant),
                  completed: challenge-completed
                }
              )
              
              (if challenge-completed
                (ok (unwrap! (update-leaderboard challenge-id tx-sender new-savings completion-time) (err u300)))
                (ok true)
              )
            )
          )
          ERR-CHALLENGE-NOT-FOUND
        )
      )
      ERR-CHALLENGE-NOT-FOUND
    )
  )
)

(define-private (update-leaderboard (challenge-id uint) (participant principal) (amount uint) (completion-time uint))
  (let (
    (current-rank (get-participant-rank challenge-id participant))
  )
    (ok (map-set challenge-leaderboard
      { challenge-id: challenge-id, rank: current-rank }
      {
        participant: participant,
        amount: amount,
        completion-time: completion-time
      }
    ))
  )
)

(define-read-only (get-participant-rank (challenge-id uint) (participant principal))
  (let (
    (participant-data (unwrap! (map-get? challenge-participants 
      { challenge-id: challenge-id, participant: participant }) u999))
  )
    (if (get completed participant-data)
      u1
      u999
    )
  )
)

(define-public (finalize-challenge (challenge-id uint))
  (let (
    (challenge (unwrap! (map-get? challenges { challenge-id: challenge-id }) ERR-CHALLENGE-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get creator challenge)) ERR-UNAUTHORIZED-CHALLENGE)
    (asserts! (>= stacks-block-height (get end-block challenge)) ERR-CHALLENGE-NOT-ENDED)
    (asserts! (get is-active challenge) ERR-CHALLENGE-NOT-ACTIVE)
    
    (ok (map-set challenges
      { challenge-id: challenge-id }
      {
        title: (get title challenge),
        description: (get description challenge),
        target-amount: (get target-amount challenge),
        reward-pool: (get reward-pool challenge),
        start-block: (get start-block challenge),
        end-block: (get end-block challenge),
        max-participants: (get max-participants challenge),
        current-participants: (get current-participants challenge),
        creator: (get creator challenge),
        is-active: false,
        challenge-type: (get challenge-type challenge)
      }
    ))
  )
)

(define-public (distribute-rewards (challenge-id uint) (winner principal))
  (let (
    (challenge (unwrap! (map-get? challenges { challenge-id: challenge-id }) ERR-CHALLENGE-NOT-FOUND))
    (winner-data (unwrap! (map-get? challenge-participants 
      { challenge-id: challenge-id, participant: winner }) ERR-CHALLENGE-NOT-FOUND))
    (current-stats (default-to 
      { challenges-joined: u0, challenges-completed: u0, total-winnings: u0, best-rank: u999, current-streak: u0 }
      (map-get? user-challenge-stats { user: winner })))
  )
    (asserts! (is-eq tx-sender (get creator challenge)) ERR-UNAUTHORIZED-CHALLENGE)
    (asserts! (not (get is-active challenge)) ERR-CHALLENGE-NOT-ENDED)
    (asserts! (get completed winner-data) ERR-CHALLENGE-NOT-ENDED)
    
    (ok (map-set user-challenge-stats
      { user: winner }
      {
        challenges-joined: (+ (get challenges-joined current-stats) u1),
        challenges-completed: (+ (get challenges-completed current-stats) u1),
        total-winnings: (+ (get total-winnings current-stats) (get reward-pool challenge)),
        best-rank: (if (< u1 (get best-rank current-stats)) u1 (get best-rank current-stats)),
        current-streak: (+ (get current-streak current-stats) u1)
      }
    ))
  )
)

(define-read-only (get-challenge (challenge-id uint))
  (map-get? challenges { challenge-id: challenge-id })
)

(define-read-only (get-challenge-participant (challenge-id uint) (participant principal))
  (map-get? challenge-participants { challenge-id: challenge-id, participant: participant })
)

(define-read-only (get-leaderboard-entry (challenge-id uint) (rank uint))
  (map-get? challenge-leaderboard { challenge-id: challenge-id, rank: rank })
)

(define-read-only (get-user-challenge-stats (user principal))
  (map-get? user-challenge-stats { user: user })
)

(define-read-only (get-active-challenges)
  (let (
    (current-challenge-count (var-get challenge-counter))
  )
    (ok current-challenge-count)
  )
)

(define-read-only (check-challenge-eligibility (challenge-id uint) (user principal))
  (let (
    (challenge (unwrap! (map-get? challenges { challenge-id: challenge-id }) ERR-CHALLENGE-NOT-FOUND))
    (existing-participation (map-get? challenge-participants 
      { challenge-id: challenge-id, participant: user }))
  )
    (ok {
      can-join: (and 
        (is-none existing-participation)
        (get is-active challenge)
        (< stacks-block-height (get end-block challenge))
        (< (get current-participants challenge) (get max-participants challenge))
      ),
      time-remaining: (if (> (get end-block challenge) stacks-block-height)
        (- (get end-block challenge) stacks-block-height)
        u0),
      spots-remaining: (- (get max-participants challenge) (get current-participants challenge))
    })
  )
)

(define-read-only (get-challenge-progress (challenge-id uint) (participant principal))
  (let (
    (challenge (unwrap! (map-get? challenges { challenge-id: challenge-id }) ERR-CHALLENGE-NOT-FOUND))
    (participant-data (unwrap! (map-get? challenge-participants 
      { challenge-id: challenge-id, participant: participant }) ERR-CHALLENGE-NOT-FOUND))
  )
    (ok {
      progress-percentage: (/ (* (get current-savings participant-data) u100) (get target-amount challenge)),
      amount-remaining: (- (get target-amount challenge) (get current-savings participant-data)),
      is-completed: (get completed participant-data),
      time-remaining: (if (> (get end-block challenge) stacks-block-height)
        (- (get end-block challenge) stacks-block-height)
        u0)
    })
  )
)
