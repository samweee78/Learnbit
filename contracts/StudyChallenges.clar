;; Study Challenges Contract for Learnbit
;; Gamified time-based study competitions with entry fees and rewards

;; Error constants
(define-constant err-unauthorized (err u200))
(define-constant err-challenge-not-found (err u201))
(define-constant err-challenge-inactive (err u202))
(define-constant err-already-participating (err u203))
(define-constant err-insufficient-balance (err u204))
(define-constant err-challenge-not-ended (err u205))
(define-constant err-invalid-duration (err u206))
(define-constant err-invalid-entry-fee (err u207))
(define-constant err-not-participant (err u208))
(define-constant err-already-claimed (err u209))

;; Challenge data structure
(define-map challenges
    uint
    {
        title: (string-ascii 64),
        description: (string-ascii 128),
        creator: principal,
        entry-fee: uint,
        prize-pool: uint,
        start-time: uint,
        duration: uint,
        max-participants: uint,
        current-participants: uint,
        active: bool,
        ended: bool
    }
)

;; Challenge participants tracking
(define-map challenge-participants
    {challenge-id: uint, participant: principal}
    {
        entry-time: uint,
        study-hours: uint,
        last-update: uint,
        claimed-reward: bool
    }
)

;; Challenge leaderboard (top 10 participants per challenge)
(define-map challenge-leaderboard
    uint
    (list 10 {participant: principal, hours: uint})
)

;; User challenge history
(define-map user-challenge-history
    principal
    (list 20 uint)
)

;; Contract variables
(define-data-var challenge-counter uint u0)
(define-data-var min-entry-fee uint u5)
(define-data-var max-challenge-duration uint u10080) ;; 1 week in minutes
(define-data-var platform-fee-rate uint u10) ;; 10% platform fee

;; Create a new study challenge
(define-public (create-challenge (title (string-ascii 64)) (description (string-ascii 128)) (entry-fee uint) (duration uint) (max-participants uint))
    (let (
        (challenge-id (+ (var-get challenge-counter) u1))
        (creator tx-sender)
    )
        (asserts! (>= entry-fee (var-get min-entry-fee)) err-invalid-entry-fee)
        (asserts! (and (> duration u0) (<= duration (var-get max-challenge-duration))) err-invalid-duration)
        (asserts! (and (> max-participants u1) (<= max-participants u50)) err-invalid-entry-fee)
        
        (map-set challenges
            challenge-id
            {
                title: title,
                description: description,
                creator: creator,
                entry-fee: entry-fee,
                prize-pool: u0,
                start-time: (+ stacks-block-height u144), ;; Start 1 hour from now
                duration: duration,
                max-participants: max-participants,
                current-participants: u0,
                active: true,
                ended: false
            }
        )
        
        (var-set challenge-counter challenge-id)
        (ok challenge-id)
    )
)

;; Join a study challenge with entry fee
(define-public (join-challenge (challenge-id uint))
    (let (
        (participant tx-sender)
        (challenge-data (unwrap! (map-get? challenges challenge-id) err-challenge-not-found))
        (entry-fee (get entry-fee challenge-data))
        (platform-fee (/ (* entry-fee (var-get platform-fee-rate)) u100))
        (prize-contribution (- entry-fee platform-fee))
    )
        (asserts! (get active challenge-data) err-challenge-inactive)
        (asserts! (< (get current-participants challenge-data) (get max-participants challenge-data)) err-challenge-not-found)
        (asserts! (< stacks-block-height (+ (get start-time challenge-data) (get duration challenge-data))) err-challenge-not-ended)
        (asserts! (is-none (map-get? challenge-participants {challenge-id: challenge-id, participant: participant})) err-already-participating)
        
        ;; Entry fee will be handled by integration with main Learnbit contract
        
        ;; Add to participants
        (map-set challenge-participants
            {challenge-id: challenge-id, participant: participant}
            {
                entry-time: stacks-block-height,
                study-hours: u0,
                last-update: stacks-block-height,
                claimed-reward: false
            }
        )
        
        ;; Update challenge data
        (map-set challenges
            challenge-id
            (merge challenge-data {
                current-participants: (+ (get current-participants challenge-data) u1),
                prize-pool: (+ (get prize-pool challenge-data) prize-contribution)
            })
        )
        
        ;; Update user history
        (let ((user-history (default-to (list) (map-get? user-challenge-history participant))))
            (map-set user-challenge-history
                participant
                (unwrap! (as-max-len? (append user-history challenge-id) u20) err-challenge-not-found)
            )
        )
        
        (ok true)
    )
)

;; Update study progress for a challenge
(define-public (update-challenge-progress (challenge-id uint) (additional-hours uint))
    (let (
        (participant tx-sender)
        (challenge-data (unwrap! (map-get? challenges challenge-id) err-challenge-not-found))
        (participant-data (unwrap! (map-get? challenge-participants {challenge-id: challenge-id, participant: participant}) err-not-participant))
    )
        (asserts! (get active challenge-data) err-challenge-inactive)
        (asserts! (< stacks-block-height (+ (get start-time challenge-data) (get duration challenge-data))) err-challenge-not-ended)
        (asserts! (> additional-hours u0) err-invalid-duration)
        
        (let ((new-total-hours (+ (get study-hours participant-data) additional-hours)))
            ;; Update participant progress
            (map-set challenge-participants
                {challenge-id: challenge-id, participant: participant}
                (merge participant-data {
                    study-hours: new-total-hours,
                    last-update: stacks-block-height
                })
            )
            
            ;; Update leaderboard and return result
            (unwrap-panic (update-leaderboard challenge-id participant new-total-hours))
            (ok new-total-hours)
        )
    )
)

;; End challenge and determine winners
(define-public (end-challenge (challenge-id uint))
    (let (
        (challenge-data (unwrap! (map-get? challenges challenge-id) err-challenge-not-found))
        (end-time (+ (get start-time challenge-data) (get duration challenge-data)))
    )
        (asserts! (>= stacks-block-height end-time) err-challenge-not-ended)
        (asserts! (not (get ended challenge-data)) err-challenge-not-found)
        
        (map-set challenges
            challenge-id
            (merge challenge-data {
                active: false,
                ended: true
            })
        )
        
        (ok true)
    )
)

;; Claim reward for top performers
(define-public (claim-challenge-reward (challenge-id uint))
    (let (
        (claimer tx-sender)
        (challenge-data (unwrap! (map-get? challenges challenge-id) err-challenge-not-found))
        (participant-data (unwrap! (map-get? challenge-participants {challenge-id: challenge-id, participant: claimer}) err-not-participant))
        (leaderboard (default-to (list) (map-get? challenge-leaderboard challenge-id)))
    )
        (asserts! (get ended challenge-data) err-challenge-not-ended)
        (asserts! (not (get claimed-reward participant-data)) err-already-claimed)
        
        (let ((participant-rank (get-participant-rank claimer leaderboard)))
            (if (< participant-rank u4) ;; Top 3 get rewards
                (let (
                    (total-prize (get prize-pool challenge-data))
                    (reward-amount (if (is-eq participant-rank u0)
                        (/ (* total-prize u50) u100) ;; 50% for 1st
                        (if (is-eq participant-rank u1)
                            (/ (* total-prize u30) u100) ;; 30% for 2nd
                            (/ (* total-prize u20) u100) ;; 20% for 3rd
                        )
                    ))
                )
                    ;; Reward distribution will be handled by integration with main Learnbit contract
                    
                    (map-set challenge-participants
                        {challenge-id: challenge-id, participant: claimer}
                        (merge participant-data {claimed-reward: true})
                    )
                    (ok reward-amount)
                )
                (ok u0) ;; No reward for participants outside top 3
            )
        )
    )
)

;; Private function to update leaderboard
(define-private (update-leaderboard (challenge-id uint) (participant principal) (hours uint))
    (let ((current-leaderboard (default-to (list) (map-get? challenge-leaderboard challenge-id))))
        (map-set challenge-leaderboard
            challenge-id
            (sort-leaderboard (append-or-update-participant current-leaderboard participant hours))
        )
        (ok true)
    )
)

;; Helper function to append or update participant in leaderboard
(define-private (append-or-update-participant (leaderboard (list 10 {participant: principal, hours: uint})) (participant principal) (hours uint))
    (let ((new-entry {participant: participant, hours: hours}))
        (if (> (len leaderboard) u0)
            (unwrap! (as-max-len? (append (filter-participant leaderboard participant) new-entry) u10) (list new-entry))
            (list new-entry)
        )
    )
)

;; Helper function to filter out existing participant
(define-private (filter-participant (leaderboard (list 10 {participant: principal, hours: uint})) (target-participant principal))
    (filter is-not-target-participant leaderboard)
)

(define-private (is-not-target-participant (entry {participant: principal, hours: uint}))
    (not (is-eq (get participant entry) tx-sender))
)

;; Helper function to sort leaderboard by hours (descending)
(define-private (sort-leaderboard (leaderboard (list 10 {participant: principal, hours: uint})))
    leaderboard ;; Simplified - in production would implement proper sorting
)

;; Helper function to get participant rank in leaderboard
(define-private (get-participant-rank (participant principal) (leaderboard (list 10 {participant: principal, hours: uint})))
    u10 ;; Simplified - would implement proper ranking logic
)

;; Read-only functions
(define-read-only (get-challenge-info (challenge-id uint))
    (map-get? challenges challenge-id)
)

(define-read-only (get-participant-info (challenge-id uint) (participant principal))
    (map-get? challenge-participants {challenge-id: challenge-id, participant: participant})
)

(define-read-only (get-challenge-leaderboard (challenge-id uint))
    (map-get? challenge-leaderboard challenge-id)
)

(define-read-only (get-user-challenges (user principal))
    (map-get? user-challenge-history user)
)

(define-read-only (get-total-challenges)
    (var-get challenge-counter)
)

(define-read-only (is-challenge-active (challenge-id uint))
    (match (map-get? challenges challenge-id)
        challenge-data (and 
            (get active challenge-data)
            (< stacks-block-height (+ (get start-time challenge-data) (get duration challenge-data)))
        )
        false
    )
)
