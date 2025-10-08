(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u200))
(define-constant ERR_GOAL_NOT_FOUND (err u201))
(define-constant ERR_INVALID_TARGET (err u202))
(define-constant ERR_GOAL_EXISTS (err u203))
(define-constant ERR_MILESTONE_EXISTS (err u204))

(define-map contribution-goals
  principal
  {
    annual-target: uint,
    lifetime-target: uint,
    current-year-contributed: uint,
    lifetime-contributed: uint,
    goal-set-at: uint,
    last-year-reset: uint
  }
)

(define-map milestones-achieved
  {user: principal, milestone-type: (string-ascii 20)}
  {
    achieved-at: uint,
    milestone-value: uint,
    reward-claimed: bool
  }
)

(define-data-var total-goals-set uint u0)

(define-public (set-contribution-goal (annual-target uint) (lifetime-target uint))
  (let ((caller tx-sender))
    (asserts! (> annual-target u0) ERR_INVALID_TARGET)
    (asserts! (> lifetime-target u0) ERR_INVALID_TARGET)
    (asserts! (>= lifetime-target annual-target) ERR_INVALID_TARGET)
    (map-set contribution-goals caller {
      annual-target: annual-target,
      lifetime-target: lifetime-target,
      current-year-contributed: u0,
      lifetime-contributed: u0,
      goal-set-at: stacks-block-height,
      last-year-reset: stacks-block-height
    })
    (var-set total-goals-set (+ (var-get total-goals-set) u1))
    (ok true)
  )
)

(define-public (record-contribution (amount uint))
  (let (
    (caller tx-sender)
    (goal-data (map-get? contribution-goals caller))
  )
    (match goal-data
      existing-goal (let (
        (blocks-since-reset (- stacks-block-height (get last-year-reset existing-goal)))
        (should-reset (>= blocks-since-reset u525600))
        (new-year-amount (if should-reset amount (+ (get current-year-contributed existing-goal) amount)))
        (new-lifetime-amount (+ (get lifetime-contributed existing-goal) amount))
        (updated-goal (merge existing-goal {
          current-year-contributed: new-year-amount,
          lifetime-contributed: new-lifetime-amount,
          last-year-reset: (if should-reset stacks-block-height (get last-year-reset existing-goal))
        }))
      )
        (map-set contribution-goals caller updated-goal)
        (try! (check-and-award-milestones caller new-year-amount new-lifetime-amount))
        (ok true)
      )
      (ok true)
    )
  )
)

(define-private (check-and-award-milestones (user principal) (year-amount uint) (lifetime-amount uint))
  (let ((goal-data (unwrap! (map-get? contribution-goals user) ERR_GOAL_NOT_FOUND)))
    (if (and (>= year-amount (get annual-target goal-data))
             (is-none (map-get? milestones-achieved {user: user, milestone-type: "annual-goal"})))
      (map-set milestones-achieved {user: user, milestone-type: "annual-goal"} {
        achieved-at: stacks-block-height,
        milestone-value: year-amount,
        reward-claimed: false
      })
      true
    )
    (if (and (>= lifetime-amount (get lifetime-target goal-data))
             (is-none (map-get? milestones-achieved {user: user, milestone-type: "lifetime-goal"})))
      (map-set milestones-achieved {user: user, milestone-type: "lifetime-goal"} {
        achieved-at: stacks-block-height,
        milestone-value: lifetime-amount,
        reward-claimed: false
      })
      true
    )
    (ok true)
  )
)

(define-read-only (get-goal-progress (user principal))
  (map-get? contribution-goals user)
)

(define-read-only (get-milestone (user principal) (milestone-type (string-ascii 20)))
  (map-get? milestones-achieved {user: user, milestone-type: milestone-type})
)

(define-read-only (calculate-goal-progress-percentage (user principal))
  (match (map-get? contribution-goals user)
    goal-data (let (
      (annual-progress (/ (* (get current-year-contributed goal-data) u100) (get annual-target goal-data)))
      (lifetime-progress (/ (* (get lifetime-contributed goal-data) u100) (get lifetime-target goal-data)))
    )
      {annual-progress: annual-progress, lifetime-progress: lifetime-progress}
    )
    {annual-progress: u0, lifetime-progress: u0}
  )
)

(define-read-only (get-total-goals-set)
  (var-get total-goals-set)
)
