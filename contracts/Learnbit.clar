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
(define-constant err-resource-not-found (err u116))
(define-constant err-invalid-price (err u117))
(define-constant err-cannot-buy-own-resource (err u118))
(define-constant err-resource-not-active (err u119))
(define-constant err-invalid-rating (err u120))
(define-constant err-already-rated (err u121))
(define-constant err-not-resource-owner (err u122))
(define-constant err-invalid-resource-data (err u123))
(define-constant err-resource-already-purchased (err u124))

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

(define-map study-resources
    uint
    {
        title: (string-ascii 128),
        description: (string-ascii 256),
        subject: (string-ascii 64),
        creator: principal,
        price: uint,
        creation-time: uint,
        total-sales: uint,
        total-ratings: uint,
        rating-sum: uint,
        active: bool
    }
)

(define-map resource-purchases
    {buyer: principal, resource-id: uint}
    {
        purchase-time: uint,
        price-paid: uint
    }
)

(define-map resource-ratings
    {rater: principal, resource-id: uint}
    {
        rating: uint,
        review-time: uint
    }
)

(define-map creator-earnings
    principal
    {
        total-earned: uint,
        total-resources: uint,
        average-rating: uint
    }
)

(define-data-var reward-rate uint u10)
(define-data-var minimum-study-time uint u30)
(define-data-var cooldown-period uint u86400)
(define-data-var max-group-size uint u20)
(define-data-var group-bonus-multiplier uint u5)
(define-data-var group-counter uint u0)
(define-data-var resource-counter uint u0)
(define-data-var platform-fee-percentage uint u5)
(define-data-var min-resource-price uint u1)

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

(define-public (create-study-resource (title (string-ascii 128)) (description (string-ascii 256)) (subject (string-ascii 64)) (price uint))
    (let (
        (creator tx-sender)
        (new-resource-id (+ (var-get resource-counter) u1))
    )
        (asserts! (is-some (map-get? students creator)) err-not-registered)
        (asserts! (> (len title) u0) err-invalid-resource-data)
        (asserts! (> (len description) u0) err-invalid-resource-data)
        (asserts! (> (len subject) u0) err-invalid-resource-data)
        (asserts! (>= price (var-get min-resource-price)) err-invalid-price)
        
        (map-set study-resources
            new-resource-id
            {
                title: title,
                description: description,
                subject: subject,
                creator: creator,
                price: price,
                creation-time: stacks-block-height,
                total-sales: u0,
                total-ratings: u0,
                rating-sum: u0,
                active: true
            }
        )
        
        (let ((current-earnings (default-to {total-earned: u0, total-resources: u0, average-rating: u0} (map-get? creator-earnings creator))))
            (map-set creator-earnings
                creator
                (merge current-earnings {
                    total-resources: (+ (get total-resources current-earnings) u1)
                })
            )
        )
        
        (var-set resource-counter new-resource-id)
        (ok new-resource-id)
    )
)

(define-public (purchase-study-resource (resource-id uint))
    (let (
        (buyer tx-sender)
        (resource-data (unwrap! (map-get? study-resources resource-id) err-resource-not-found))
        (price (get price resource-data))
        (creator (get creator resource-data))
        (platform-fee (/ (* price (var-get platform-fee-percentage)) u100))
        (creator-payment (- price platform-fee))
    )
        (asserts! (is-some (map-get? students buyer)) err-not-registered)
        (asserts! (get active resource-data) err-resource-not-active)
        (asserts! (not (is-eq buyer creator)) err-cannot-buy-own-resource)
        (asserts! (is-none (map-get? resource-purchases {buyer: buyer, resource-id: resource-id})) err-resource-already-purchased)
        (asserts! (>= (ft-get-balance learnbit buyer) price) err-insufficient-balance)
        
        (try! (transfer-tokens buyer creator creator-payment))
        (try! (transfer-tokens buyer contract-owner platform-fee))
        
        (map-set resource-purchases
            {buyer: buyer, resource-id: resource-id}
            {
                purchase-time: stacks-block-height,
                price-paid: price
            }
        )
        
        (map-set study-resources
            resource-id
            (merge resource-data {
                total-sales: (+ (get total-sales resource-data) u1)
            })
        )
        
        (let ((current-creator-earnings (default-to {total-earned: u0, total-resources: u0, average-rating: u0} (map-get? creator-earnings creator))))
            (map-set creator-earnings
                creator
                (merge current-creator-earnings {
                    total-earned: (+ (get total-earned current-creator-earnings) creator-payment)
                })
            )
        )
        
        (ok true)
    )
)

(define-public (rate-study-resource (resource-id uint) (rating uint))
    (let (
        (rater tx-sender)
        (resource-data (unwrap! (map-get? study-resources resource-id) err-resource-not-found))
    )
        (asserts! (is-some (map-get? students rater)) err-not-registered)
        (asserts! (and (>= rating u1) (<= rating u5)) err-invalid-rating)
        (asserts! (is-some (map-get? resource-purchases {buyer: rater, resource-id: resource-id})) err-resource-not-found)
        (asserts! (is-none (map-get? resource-ratings {rater: rater, resource-id: resource-id})) err-already-rated)
        
        (map-set resource-ratings
            {rater: rater, resource-id: resource-id}
            {
                rating: rating,
                review-time: stacks-block-height
            }
        )
        
        (let (
            (new-total-ratings (+ (get total-ratings resource-data) u1))
            (new-rating-sum (+ (get rating-sum resource-data) rating))
            (new-average-rating (/ new-rating-sum new-total-ratings))
        )
            (map-set study-resources
                resource-id
                (merge resource-data {
                    total-ratings: new-total-ratings,
                    rating-sum: new-rating-sum
                })
            )
            
            (let ((creator-data (default-to {total-earned: u0, total-resources: u0, average-rating: u0} (map-get? creator-earnings (get creator resource-data)))))
                (map-set creator-earnings
                    (get creator resource-data)
                    (merge creator-data {
                        average-rating: new-average-rating
                    })
                )
            )
        )
        
        (ok true)
    )
)

(define-public (deactivate-study-resource (resource-id uint))
    (let (
        (caller tx-sender)
        (resource-data (unwrap! (map-get? study-resources resource-id) err-resource-not-found))
    )
        (asserts! (is-eq caller (get creator resource-data)) err-not-resource-owner)
        (asserts! (get active resource-data) err-resource-not-active)
        
        (map-set study-resources
            resource-id
            (merge resource-data {
                active: false
            })
        )
        
        (ok true)
    )
)

(define-public (update-resource-price (resource-id uint) (new-price uint))
    (let (
        (caller tx-sender)
        (resource-data (unwrap! (map-get? study-resources resource-id) err-resource-not-found))
    )
        (asserts! (is-eq caller (get creator resource-data)) err-not-resource-owner)
        (asserts! (get active resource-data) err-resource-not-active)
        (asserts! (>= new-price (var-get min-resource-price)) err-invalid-price)
        
        (map-set study-resources
            resource-id
            (merge resource-data {
                price: new-price
            })
        )
        
        (ok true)
    )
)

(define-public (search-resources-by-subject (subject (string-ascii 64)))
    (ok (var-get resource-counter))
)

(define-read-only (get-resource-info (resource-id uint))
    (ok (map-get? study-resources resource-id))
)

(define-read-only (get-resource-purchase (buyer principal) (resource-id uint))
    (ok (map-get? resource-purchases {buyer: buyer, resource-id: resource-id}))
)

(define-read-only (get-resource-rating (rater principal) (resource-id uint))
    (ok (map-get? resource-ratings {rater: rater, resource-id: resource-id}))
)

(define-read-only (get-creator-earnings (creator principal))
    (ok (map-get? creator-earnings creator))
)

(define-read-only (get-resource-average-rating (resource-id uint))
    (let ((resource-data (map-get? study-resources resource-id)))
        (match resource-data
            resource-info (if (> (get total-ratings resource-info) u0)
                (ok (some (/ (get rating-sum resource-info) (get total-ratings resource-info))))
                (ok none)
            )
            (ok none)
        )
    )
)

(define-read-only (has-purchased-resource (buyer principal) (resource-id uint))
    (ok (is-some (map-get? resource-purchases {buyer: buyer, resource-id: resource-id})))
)

(define-read-only (get-total-resources)
    (ok (var-get resource-counter))
)

(define-read-only (get-platform-stats)
    (ok {
        total-resources: (var-get resource-counter),
        platform-fee: (var-get platform-fee-percentage),
        min-price: (var-get min-resource-price)
    })
)

