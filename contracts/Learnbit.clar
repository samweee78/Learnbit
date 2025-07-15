(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-registered (err u101))
(define-constant err-already-registered (err u102))
(define-constant err-invalid-hours (err u103))
(define-constant err-cooldown-active (err u104))
(define-constant err-insufficient-balance (err u105))

(define-fungible-token learnbit)

(define-map students 
    principal 
    {
        total-hours: uint,
        total-rewards: uint,
        last-study-time: uint,
        study-streak: uint,
        verification-status: bool
    }
)

(define-map study-sessions
    principal
    {
        start-time: uint,
        subject: (string-ascii 64),
        duration: uint,
        verified: bool
    }
)

(define-data-var reward-rate uint u10)
(define-data-var minimum-study-time uint u30)
(define-data-var cooldown-period uint u86400)

(define-public (register-student)
    (let ((student tx-sender))
        (asserts! (is-none (map-get? students student)) err-already-registered)
        (ok (map-set students 
            student
            {
                total-hours: u0,
                total-rewards: u0,
                last-study-time: u0,
                study-streak: u0,
                verification-status: false
            }
        ))
    )
)

(define-public (start-study-session (subject (string-ascii 64)))
    (let ((student tx-sender))
        (asserts! (is-some (map-get? students student)) err-not-registered)
        (ok (map-set study-sessions
            student
            {
                start-time: stacks-block-height,
                subject: subject,
                duration: u0,
                verified: false
            }
        ))
    )
)

(define-public (end-study-session (hours uint))
    (let (
        (student tx-sender)
        (student-data (unwrap! (map-get? students student) err-not-registered))
        (session-data (unwrap! (map-get? study-sessions student) err-not-registered))
    )
        (asserts! (> hours u0) err-invalid-hours)
        (asserts! (>= (- stacks-block-height (get last-study-time student-data)) (var-get cooldown-period)) err-cooldown-active)
        
        (let ((rewards (* hours (var-get reward-rate))))
            (try! (mint-tokens student rewards))
            (map-set students
                student
                {
                    total-hours: (+ (get total-hours student-data) hours),
                    total-rewards: (+ (get total-rewards student-data) rewards),
                    last-study-time: stacks-block-height,
                    study-streak: (+ (get study-streak student-data) u1),
                    verification-status: (get verification-status student-data)
                }
            )
            (ok rewards)
        )
    )
)

(define-public (verify-student (student principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (is-some (map-get? students student)) err-not-registered)
        (ok (map-set students
            student
            (merge (unwrap! (map-get? students student) err-not-registered)
                { verification-status: true }
            )
        ))
    )
)

(define-public (transfer-rewards (recipient principal) (amount uint))
    (let ((sender tx-sender))
        (asserts! (is-some (map-get? students sender)) err-not-registered)
        (asserts! (is-some (map-get? students recipient)) err-not-registered)
        (try! (transfer-tokens sender recipient amount))
        (ok true)
    )
)

(define-private (mint-tokens (recipient principal) (amount uint))
    (ft-mint? learnbit amount recipient)
)

(define-private (transfer-tokens (sender principal) (recipient principal) (amount uint))
    (ft-transfer? learnbit amount sender recipient)
)

(define-read-only (get-student-info (student principal))
    (ok (map-get? students student))
)

(define-read-only (get-session-info (student principal))
    (ok (map-get? study-sessions student))
)

(define-read-only (get-balance (student principal))
    (ok (ft-get-balance learnbit student))
)

(define-public (update-reward-rate (new-rate uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (var-set reward-rate new-rate))
    )
)

(define-public (update-minimum-study-time (new-time uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (var-set minimum-study-time new-time))
    )
)

(define-public (update-cooldown-period (new-period uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (var-set cooldown-period new-period))
    )
)