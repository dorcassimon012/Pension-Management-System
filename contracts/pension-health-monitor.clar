(define-constant ERR_ACCOUNT_NOT_FOUND (err u300))
(define-constant ERR_INVALID_THRESHOLD (err u301))
(define-constant RETIREMENT_AGE u65)
(define-constant BLOCKS_PER_YEAR u52560)
(define-constant RECOMMENDED_BALANCE_MULTIPLIER u50000)

(define-data-var health-check-count uint u0)
(define-data-var average-system-health uint u0)

(define-map health-assessments
  principal
  {
    health-score: uint,
    risk-level: (string-ascii 10),
    assessed-at: uint,
    contribution-consistency: uint,
    savings-adequacy: uint,
    staking-participation: uint
  }
)

(define-map alert-preferences
  principal
  {
    low-health-threshold: uint,
    notify-on-decline: bool,
    last-alert: uint
  }
)

(define-public (assess-pension-health (user principal))
  (let (
    (account-data (unwrap! (contract-call? .Pension-Management-System get-account-info user) ERR_ACCOUNT_NOT_FOUND))
    (current-age (contract-call? .Pension-Management-System calculate-current-age user))
    (years-to-retirement (if (>= current-age RETIREMENT_AGE) u0 (- RETIREMENT_AGE current-age)))
    (contribution-score (calculate-contribution-score account-data))
    (savings-score (calculate-savings-score account-data current-age years-to-retirement))
    (staking-score (calculate-staking-score account-data))
    (total-score (/ (+ contribution-score savings-score staking-score) u3))
    (risk-category (determine-risk-level total-score))
  )
    (map-set health-assessments user {
      health-score: total-score,
      risk-level: risk-category,
      assessed-at: stacks-block-height,
      contribution-consistency: contribution-score,
      savings-adequacy: savings-score,
      staking-participation: staking-score
    })
    (var-set health-check-count (+ (var-get health-check-count) u1))
    (update-system-average total-score)
    (ok {score: total-score, risk: risk-category})
  )
)

(define-private (calculate-contribution-score (account {balance: uint, age: uint, last-contribution: uint, retirement-status: bool, staking-start: uint, total-contributions: uint, total-rewards: uint}))
  (let (
    (blocks-since-contrib (- stacks-block-height (get last-contribution account)))
    (total-contrib (get total-contributions account))
  )
    (if (is-eq total-contrib u0)
      u0
      (if (<= blocks-since-contrib u1440)
        u100
        (if (<= blocks-since-contrib u4320)
          u75
          (if (<= blocks-since-contrib u52560)
            u50
            u25
          )
        )
      )
    )
  )
)

(define-private (calculate-savings-score (account {balance: uint, age: uint, last-contribution: uint, retirement-status: bool, staking-start: uint, total-contributions: uint, total-rewards: uint}) (current-age uint) (years-left uint))
  (let (
    (recommended-balance (* current-age RECOMMENDED_BALANCE_MULTIPLIER))
    (actual-balance (get balance account))
  )
    (if (is-eq years-left u0)
      u100
      (if (is-eq recommended-balance u0)
        u0
        (if (>= actual-balance recommended-balance)
          u100
          (/ (* actual-balance u100) recommended-balance)
        )
      )
    )
  )
)

(define-private (calculate-staking-score (account {balance: uint, age: uint, last-contribution: uint, retirement-status: bool, staking-start: uint, total-contributions: uint, total-rewards: uint}))
  (let (
    (total-rewards (get total-rewards account))
    (staking-start (get staking-start account))
  )
    (if (> staking-start u0)
      (if (> total-rewards u0)
        u100
        u60
      )
      u0
    )
  )
)

(define-private (determine-risk-level (score uint))
  (if (>= score u80)
    "HEALTHY"
    (if (>= score u60)
      "MODERATE"
      (if (>= score u40)
        "CAUTION"
        "HIGH-RISK"
      )
    )
  )
)

(define-private (update-system-average (new-score uint))
  (let (
    (count (var-get health-check-count))
    (current-avg (var-get average-system-health))
  )
    (if (> count u0)
      (let (
        (total-score (+ (* current-avg (- count u1)) new-score))
        (new-avg (/ total-score count))
      )
        (var-set average-system-health new-avg)
        true
      )
      true
    )
  )
)

(define-public (set-alert-preferences (threshold uint) (notify bool))
  (begin
    (asserts! (and (> threshold u0) (<= threshold u100)) ERR_INVALID_THRESHOLD)
    (map-set alert-preferences tx-sender {
      low-health-threshold: threshold,
      notify-on-decline: notify,
      last-alert: stacks-block-height
    })
    (ok true)
  )
)

(define-read-only (get-health-assessment (user principal))
  (map-get? health-assessments user)
)

(define-read-only (get-system-health-average)
  (var-get average-system-health)
)

(define-read-only (compare-to-average (user principal))
  (match (map-get? health-assessments user)
    assessment (let (
      (user-score (get health-score assessment))
      (avg-score (var-get average-system-health))
      (difference (if (>= user-score avg-score)
        (- user-score avg-score)
        (- avg-score user-score)
      ))
      (performance (if (>= user-score avg-score) "above" "below"))
    )
      (some {user-score: user-score, system-average: avg-score, difference: difference, performance: performance})
    )
    none
  )
)

(define-read-only (get-total-assessments)
  (var-get health-check-count)
)
