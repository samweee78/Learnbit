(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-registered (err u101))
(define-constant err-already-registered (err u102))
(define-constant err-invalid-hours (err u103))
(define-constant err-cooldown-active (err u104))
(define-constant err-insufficient-balance (err u105))
(define-constant err-group-not-found (err u106))
(define-constant err-not-group-member (err u107))
(define-constant err-already-group-member (err u108))
(define-constant err-group-limit-reached (err u109))
(define-constant err-invalid-group-name (err u110))
(define-constant err-group-already-exists (err u111))
(define-constant err-not-group-leader (err u112))
(define-constant err-group-goal-not-found (err u113))
(define-constant err-goal-already-completed (err u114))
(define-constant err-invalid-goal-hours (err u115))

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

(define-map study-groups
    uint
    {
        group-name: (string-ascii 64),
        leader: principal,
        members: (list 20 principal),
        member-count: uint,
        total-study-hours: uint,
        creation-time: uint,
        active: bool
    }
)

(define-map group-memberships
    principal
    {
        group-id: uint,
        joined-time: uint,
        contribution-hours: uint
    }
)

(define-map group-goals
    {group-id: uint, goal-id: uint}
    {
        target-hours: uint,
        deadline: uint,
        completed: bool,
        completion-time: uint,
        reward-per-member: uint
    }
)

(define-map group-goal-counter
    uint
    uint
)

(define-data-var reward-rate uint u10)
(define-data-var minimum-study-time uint u30)
(define-data-var cooldown-period uint u86400)
(define-data-var max-group-size uint u20)
(define-data-var group-bonus-multiplier uint u5)
(define-data-var group-counter uint u0)

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

(define-public (create-study-group (group-name (string-ascii 64)))
    (let (
        (creator tx-sender)
        (new-group-id (+ (var-get group-counter) u1))
    )
        (asserts! (is-some (map-get? students creator)) err-not-registered)
        (asserts! (> (len group-name) u0) err-invalid-group-name)
        (asserts! (is-none (map-get? group-memberships creator)) err-already-group-member)
        
        (map-set study-groups
            new-group-id
            {
                group-name: group-name,
                leader: creator,
                members: (list creator),
                member-count: u1,
                total-study-hours: u0,
                creation-time: stacks-block-height,
                active: true
            }
        )
        
        (map-set group-memberships
            creator
            {
                group-id: new-group-id,
                joined-time: stacks-block-height,
                contribution-hours: u0
            }
        )
        
        (map-set group-goal-counter new-group-id u0)
        (var-set group-counter new-group-id)
        (ok new-group-id)
    )
)

(define-public (join-study-group (group-id uint))
    (let (
        (joiner tx-sender)
        (group-data (unwrap! (map-get? study-groups group-id) err-group-not-found))
    )
        (asserts! (is-some (map-get? students joiner)) err-not-registered)
        (asserts! (is-none (map-get? group-memberships joiner)) err-already-group-member)
        (asserts! (get active group-data) err-group-not-found)
        (asserts! (< (get member-count group-data) (var-get max-group-size)) err-group-limit-reached)
        
        (map-set study-groups
            group-id
            (merge group-data {
                members: (unwrap! (as-max-len? (append (get members group-data) joiner) u20) err-group-limit-reached),
                member-count: (+ (get member-count group-data) u1)
            })
        )
        
        (map-set group-memberships
            joiner
            {
                group-id: group-id,
                joined-time: stacks-block-height,
                contribution-hours: u0
            }
        )
        
        (ok true)
    )
)

(define-public (leave-study-group)
    (let (
        (leaver tx-sender)
        (membership-data (unwrap! (map-get? group-memberships leaver) err-not-group-member))
        (group-id (get group-id membership-data))
        (group-data (unwrap! (map-get? study-groups group-id) err-group-not-found))
    )
        (asserts! (not (is-eq leaver (get leader group-data))) err-not-group-leader)
        
        (map-set study-groups
            group-id
            (merge group-data {
                members: (filter is-not-leaver (get members group-data)),
                member-count: (- (get member-count group-data) u1)
            })
        )
        
        (map-delete group-memberships leaver)
        (ok true)
    )
)

(define-public (record-group-study-hours (hours uint))
    (let (
        (student tx-sender)
        (membership-data (unwrap! (map-get? group-memberships student) err-not-group-member))
        (group-id (get group-id membership-data))
        (group-data (unwrap! (map-get? study-groups group-id) err-group-not-found))
        (bonus-rewards (* hours (var-get group-bonus-multiplier)))
    )
        (asserts! (is-some (map-get? students student)) err-not-registered)
        (asserts! (> hours u0) err-invalid-hours)
        (asserts! (get active group-data) err-group-not-found)
        
        (try! (mint-tokens student bonus-rewards))
        
        (map-set group-memberships
            student
            (merge membership-data {
                contribution-hours: (+ (get contribution-hours membership-data) hours)
            })
        )
        
        (map-set study-groups
            group-id
            (merge group-data {
                total-study-hours: (+ (get total-study-hours group-data) hours)
            })
        )
        
        (ok bonus-rewards)
    )
)

(define-public (create-group-goal (group-id uint) (target-hours uint) (deadline uint) (reward-per-member uint))
    (let (
        (creator tx-sender)
        (group-data (unwrap! (map-get? study-groups group-id) err-group-not-found))
        (current-goal-count (default-to u0 (map-get? group-goal-counter group-id)))
        (new-goal-id (+ current-goal-count u1))
    )
        (asserts! (is-eq creator (get leader group-data)) err-not-group-leader)
        (asserts! (> target-hours u0) err-invalid-goal-hours)
        (asserts! (> deadline stacks-block-height) err-invalid-goal-hours)
        (asserts! (get active group-data) err-group-not-found)
        
        (map-set group-goals
            {group-id: group-id, goal-id: new-goal-id}
            {
                target-hours: target-hours,
                deadline: deadline,
                completed: false,
                completion-time: u0,
                reward-per-member: reward-per-member
            }
        )
        
        (map-set group-goal-counter group-id new-goal-id)
        (ok new-goal-id)
    )
)

(define-public (complete-group-goal (group-id uint) (goal-id uint))
    (let (
        (completer tx-sender)
        (group-data (unwrap! (map-get? study-groups group-id) err-group-not-found))
        (goal-data (unwrap! (map-get? group-goals {group-id: group-id, goal-id: goal-id}) err-group-goal-not-found))
    )
        (asserts! (is-eq completer (get leader group-data)) err-not-group-leader)
        (asserts! (not (get completed goal-data)) err-goal-already-completed)
        (asserts! (>= (get total-study-hours group-data) (get target-hours goal-data)) err-invalid-goal-hours)
        (asserts! (<= stacks-block-height (get deadline goal-data)) err-invalid-goal-hours)
        
        (map-set group-goals
            {group-id: group-id, goal-id: goal-id}
            (merge goal-data {
                completed: true,
                completion-time: stacks-block-height
            })
        )
        
        (try! (distribute-goal-rewards group-id goal-id))
        (ok true)
    )
)

(define-private (distribute-goal-rewards (group-id uint) (goal-id uint))
    (let (
        (group-data (unwrap! (map-get? study-groups group-id) err-group-not-found))
        (goal-data (unwrap! (map-get? group-goals {group-id: group-id, goal-id: goal-id}) err-group-goal-not-found))
        (reward-amount (get reward-per-member goal-data))
        (members (get members group-data))
    )
        (begin
            (if (> (len members) u0) (try! (mint-tokens (unwrap-panic (element-at members u0)) reward-amount)) true)
            (if (> (len members) u1) (try! (mint-tokens (unwrap-panic (element-at members u1)) reward-amount)) true)
            (if (> (len members) u2) (try! (mint-tokens (unwrap-panic (element-at members u2)) reward-amount)) true)
            (if (> (len members) u3) (try! (mint-tokens (unwrap-panic (element-at members u3)) reward-amount)) true)
            (if (> (len members) u4) (try! (mint-tokens (unwrap-panic (element-at members u4)) reward-amount)) true)
            (if (> (len members) u5) (try! (mint-tokens (unwrap-panic (element-at members u5)) reward-amount)) true)
            (if (> (len members) u6) (try! (mint-tokens (unwrap-panic (element-at members u6)) reward-amount)) true)
            (if (> (len members) u7) (try! (mint-tokens (unwrap-panic (element-at members u7)) reward-amount)) true)
            (if (> (len members) u8) (try! (mint-tokens (unwrap-panic (element-at members u8)) reward-amount)) true)
            (if (> (len members) u9) (try! (mint-tokens (unwrap-panic (element-at members u9)) reward-amount)) true)
            (if (> (len members) u10) (try! (mint-tokens (unwrap-panic (element-at members u10)) reward-amount)) true)
            (if (> (len members) u11) (try! (mint-tokens (unwrap-panic (element-at members u11)) reward-amount)) true)
            (if (> (len members) u12) (try! (mint-tokens (unwrap-panic (element-at members u12)) reward-amount)) true)
            (if (> (len members) u13) (try! (mint-tokens (unwrap-panic (element-at members u13)) reward-amount)) true)
            (if (> (len members) u14) (try! (mint-tokens (unwrap-panic (element-at members u14)) reward-amount)) true)
            (if (> (len members) u15) (try! (mint-tokens (unwrap-panic (element-at members u15)) reward-amount)) true)
            (if (> (len members) u16) (try! (mint-tokens (unwrap-panic (element-at members u16)) reward-amount)) true)
            (if (> (len members) u17) (try! (mint-tokens (unwrap-panic (element-at members u17)) reward-amount)) true)
            (if (> (len members) u18) (try! (mint-tokens (unwrap-panic (element-at members u18)) reward-amount)) true)
            (if (> (len members) u19) (try! (mint-tokens (unwrap-panic (element-at members u19)) reward-amount)) true)
            (ok true)
        )
    )
)

(define-private (is-not-leaver (member principal))
    (not (is-eq member tx-sender))
)

(define-read-only (get-group-info (group-id uint))
    (ok (map-get? study-groups group-id))
)

(define-read-only (get-group-membership (student principal))
    (ok (map-get? group-memberships student))
)

(define-read-only (get-group-goal (group-id uint) (goal-id uint))
    (ok (map-get? group-goals {group-id: group-id, goal-id: goal-id}))
)

(define-read-only (get-group-goal-count (group-id uint))
    (ok (map-get? group-goal-counter group-id))
)

(define-read-only (get-total-groups)
    (ok (var-get group-counter))
)