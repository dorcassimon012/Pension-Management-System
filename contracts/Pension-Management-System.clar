(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_AMOUNT (err u101))
(define-constant ERR_ACCOUNT_NOT_FOUND (err u102))
(define-constant ERR_INSUFFICIENT_BALANCE (err u103))
(define-constant ERR_NOT_RETIREMENT_AGE (err u104))
(define-constant ERR_ALREADY_RETIRED (err u105))
(define-constant ERR_INVALID_AGE (err u106))
(define-constant ERR_STAKING_PERIOD_NOT_MET (err u107))

(define-constant RETIREMENT_AGE u65)
(define-constant MIN_STAKING_PERIOD u52560)
(define-constant STAKING_REWARD_RATE u5)

(define-data-var total-staked uint u0)
(define-data-var total-accounts uint u0)

(define-map pension-accounts
  principal
  {
    balance: uint,
    age: uint,
    last-contribution: uint,
    retirement-status: bool,
    staking-start: uint,
    total-contributions: uint,
    total-rewards: uint
  }
)

(define-map staking-pools
  uint
  {
    total-amount: uint,
    participants: uint,
    reward-rate: uint,
    created-at: uint
  }
)

(define-map user-stakes
  {user: principal, pool-id: uint}
  {
    amount: uint,
    staked-at: uint,
    last-reward-claim: uint
  }
)

(define-data-var next-pool-id uint u1)

(define-public (create-pension-account (age uint))
  (let ((caller tx-sender))
    (asserts! (> age u18) ERR_INVALID_AGE)
    (asserts! (< age RETIREMENT_AGE) ERR_INVALID_AGE)
    (asserts! (is-none (map-get? pension-accounts caller)) ERR_UNAUTHORIZED)
    (map-set pension-accounts caller {
      balance: u0,
      age: age,
      last-contribution: stacks-block-height,
      retirement-status: false,
      staking-start: u0,
      total-contributions: u0,
      total-rewards: u0
    })
    (var-set total-accounts (+ (var-get total-accounts) u1))
    (ok true)
  )
)

(define-public (contribute (amount uint))
  (let (
    (caller tx-sender)
    (account (unwrap! (map-get? pension-accounts caller) ERR_ACCOUNT_NOT_FOUND))
  )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? amount caller (as-contract tx-sender)))
    (map-set pension-accounts caller (merge account {
      balance: (+ (get balance account) amount),
      last-contribution: stacks-block-height,
      total-contributions: (+ (get total-contributions account) amount)
    }))
    (ok amount)
  )
)

(define-public (create-staking-pool (initial-amount uint))
  (let (
    (caller tx-sender)
    (pool-id (var-get next-pool-id))
    (account (unwrap! (map-get? pension-accounts caller) ERR_ACCOUNT_NOT_FOUND))
  )
    (asserts! (> initial-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= (get balance account) initial-amount) ERR_INSUFFICIENT_BALANCE)
    (map-set pension-accounts caller (merge account {
      balance: (- (get balance account) initial-amount),
      staking-start: stacks-block-height
    }))
    (map-set staking-pools pool-id {
      total-amount: initial-amount,
      participants: u1,
      reward-rate: STAKING_REWARD_RATE,
      created-at: stacks-block-height
    })
    (map-set user-stakes {user: caller, pool-id: pool-id} {
      amount: initial-amount,
      staked-at: stacks-block-height,
      last-reward-claim: stacks-block-height
    })
    (var-set next-pool-id (+ pool-id u1))
    (var-set total-staked (+ (var-get total-staked) initial-amount))
    (ok pool-id)
  )
)

(define-public (join-staking-pool (pool-id uint) (amount uint))
  (let (
    (caller tx-sender)
    (account (unwrap! (map-get? pension-accounts caller) ERR_ACCOUNT_NOT_FOUND))
    (pool (unwrap! (map-get? staking-pools pool-id) ERR_ACCOUNT_NOT_FOUND))
  )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= (get balance account) amount) ERR_INSUFFICIENT_BALANCE)
    (map-set pension-accounts caller (merge account {
      balance: (- (get balance account) amount),
      staking-start: (if (is-eq (get staking-start account) u0) stacks-block-height (get staking-start account))
    }))
    (map-set staking-pools pool-id (merge pool {
      total-amount: (+ (get total-amount pool) amount),
      participants: (+ (get participants pool) u1)
    }))
    (map-set user-stakes {user: caller, pool-id: pool-id} {
      amount: amount,
      staked-at: stacks-block-height,
      last-reward-claim: stacks-block-height
    })
    (var-set total-staked (+ (var-get total-staked) amount))
    (ok true)
  )
)

(define-public (claim-staking-rewards (pool-id uint))
  (let (
    (caller tx-sender)
    (account (unwrap! (map-get? pension-accounts caller) ERR_ACCOUNT_NOT_FOUND))
    (stake (unwrap! (map-get? user-stakes {user: caller, pool-id: pool-id}) ERR_ACCOUNT_NOT_FOUND))
    (pool (unwrap! (map-get? staking-pools pool-id) ERR_ACCOUNT_NOT_FOUND))
    (blocks-staked (- stacks-block-height (get last-reward-claim stake)))
    (reward-amount (/ (* (get amount stake) STAKING_REWARD_RATE blocks-staked) u100000))
  )
    (asserts! (> blocks-staked u0) ERR_INVALID_AMOUNT)
    (map-set pension-accounts caller (merge account {
      balance: (+ (get balance account) reward-amount),
      total-rewards: (+ (get total-rewards account) reward-amount)
    }))
    (map-set user-stakes {user: caller, pool-id: pool-id} (merge stake {
      last-reward-claim: stacks-block-height
    }))
    (ok reward-amount)
  )
)

(define-public (withdraw-from-staking (pool-id uint) (amount uint))
  (let (
    (caller tx-sender)
    (account (unwrap! (map-get? pension-accounts caller) ERR_ACCOUNT_NOT_FOUND))
    (stake (unwrap! (map-get? user-stakes {user: caller, pool-id: pool-id}) ERR_ACCOUNT_NOT_FOUND))
    (pool (unwrap! (map-get? staking-pools pool-id) ERR_ACCOUNT_NOT_FOUND))
    (blocks-staked (- stacks-block-height (get staked-at stake)))
  )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= (get amount stake) amount) ERR_INSUFFICIENT_BALANCE)
    (asserts! (>= blocks-staked MIN_STAKING_PERIOD) ERR_STAKING_PERIOD_NOT_MET)
    (try! (claim-staking-rewards pool-id))
    (map-set pension-accounts caller (merge account {
      balance: (+ (get balance account) amount)
    }))
    (map-set staking-pools pool-id (merge pool {
      total-amount: (- (get total-amount pool) amount)
    }))
    (map-set user-stakes {user: caller, pool-id: pool-id} (merge stake {
      amount: (- (get amount stake) amount)
    }))
    (var-set total-staked (- (var-get total-staked) amount))
    (ok amount)
  )
)

(define-public (retire)
  (let (
    (caller tx-sender)
    (account (unwrap! (map-get? pension-accounts caller) ERR_ACCOUNT_NOT_FOUND))
    (current-age (+ (get age account) (/ (- stacks-block-height (get last-contribution account)) u52560)))
  )
    (asserts! (>= current-age RETIREMENT_AGE) ERR_NOT_RETIREMENT_AGE)
    (asserts! (not (get retirement-status account)) ERR_ALREADY_RETIRED)
    (map-set pension-accounts caller (merge account {
      retirement-status: true
    }))
    (ok true)
  )
)

(define-public (withdraw-pension (amount uint))
  (let (
    (caller tx-sender)
    (account (unwrap! (map-get? pension-accounts caller) ERR_ACCOUNT_NOT_FOUND))
  )
    (asserts! (get retirement-status account) ERR_NOT_RETIREMENT_AGE)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= (get balance account) amount) ERR_INSUFFICIENT_BALANCE)
    (try! (as-contract (stx-transfer? amount tx-sender caller)))
    (map-set pension-accounts caller (merge account {
      balance: (- (get balance account) amount)
    }))
    (ok amount)
  )
)

(define-public (emergency-withdraw (amount uint))
  (let (
    (caller tx-sender)
    (account (unwrap! (map-get? pension-accounts caller) ERR_ACCOUNT_NOT_FOUND))
    (penalty (/ (* amount u20) u100))
    (withdrawal-amount (- amount penalty))
  )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= (get balance account) amount) ERR_INSUFFICIENT_BALANCE)
    (try! (as-contract (stx-transfer? withdrawal-amount tx-sender caller)))
    (map-set pension-accounts caller (merge account {
      balance: (- (get balance account) amount)
    }))
    (ok withdrawal-amount)
  )
)

(define-read-only (get-account-info (user principal))
  (map-get? pension-accounts user)
)

(define-read-only (get-staking-pool-info (pool-id uint))
  (map-get? staking-pools pool-id)
)

(define-read-only (get-user-stake-info (user principal) (pool-id uint))
  (map-get? user-stakes {user: user, pool-id: pool-id})
)

(define-read-only (get-total-staked)
  (var-get total-staked)
)

(define-read-only (get-total-accounts)
  (var-get total-accounts)
)

(define-read-only (calculate-current-age (user principal))
  (match (map-get? pension-accounts user)
    account (+ (get age account) (/ (- stacks-block-height (get last-contribution account)) u52560))
    u0
  )
)

(define-read-only (calculate-potential-rewards (user principal) (pool-id uint))
  (match (map-get? user-stakes {user: user, pool-id: pool-id})
    stake (let (
      (blocks-since-claim (- stacks-block-height (get last-reward-claim stake)))
    )
      (/ (* (get amount stake) STAKING_REWARD_RATE blocks-since-claim) u100000)
    )
    u0
  )
)

(define-read-only (is-eligible-for-retirement (user principal))
  (let ((current-age (calculate-current-age user)))
    (>= current-age RETIREMENT_AGE)
  )
)

(define-constant ERR_NOT_BENEFICIARY (err u108))
(define-constant ERR_BENEFICIARY_EXISTS (err u109))

(define-map pension-beneficiaries
  principal
  {
    beneficiary: principal,
    inheritance-percentage: uint,
    set-at: uint,
    active: bool
  }
)

(define-public (set-beneficiary (beneficiary principal) (percentage uint))
  (let ((caller tx-sender))
    (asserts! (is-some (map-get? pension-accounts caller)) ERR_ACCOUNT_NOT_FOUND)
    (asserts! (and (> percentage u0) (<= percentage u100)) ERR_INVALID_AMOUNT)
    (asserts! (not (is-eq caller beneficiary)) ERR_UNAUTHORIZED)
    (map-set pension-beneficiaries caller {
      beneficiary: beneficiary,
      inheritance-percentage: percentage,
      set-at: stacks-block-height,
      active: true
    })
    (ok true)
  )
)

(define-public (remove-beneficiary)
  (let ((caller tx-sender))
    (asserts! (is-some (map-get? pension-beneficiaries caller)) ERR_ACCOUNT_NOT_FOUND)
    (map-delete pension-beneficiaries caller)
    (ok true)
  )
)

(define-public (beneficiary-emergency-withdraw (account-holder principal))
  (let (
    (caller tx-sender)
    (beneficiary-info (unwrap! (map-get? pension-beneficiaries account-holder) ERR_NOT_BENEFICIARY))
    (account (unwrap! (map-get? pension-accounts account-holder) ERR_ACCOUNT_NOT_FOUND))
    (withdrawal-amount (/ (* (get balance account) (get inheritance-percentage beneficiary-info)) u100))
  )
    (asserts! (is-eq caller (get beneficiary beneficiary-info)) ERR_NOT_BENEFICIARY)
    (asserts! (get active beneficiary-info) ERR_NOT_BENEFICIARY)
    (asserts! (> withdrawal-amount u0) ERR_INSUFFICIENT_BALANCE)
    (try! (as-contract (stx-transfer? withdrawal-amount tx-sender caller)))
    (map-set pension-accounts account-holder (merge account {
      balance: (- (get balance account) withdrawal-amount)
    }))
    (map-set pension-beneficiaries account-holder (merge beneficiary-info {
      active: false
    }))
    (ok withdrawal-amount)
  )
)

(define-read-only (get-beneficiary-info (account-holder principal))
  (map-get? pension-beneficiaries account-holder)
)

(define-read-only (is-beneficiary-of (account-holder principal) (potential-beneficiary principal))
  (match (map-get? pension-beneficiaries account-holder)
    beneficiary-info (and 
      (is-eq (get beneficiary beneficiary-info) potential-beneficiary)
      (get active beneficiary-info)
    )
    false
  )
)