;; Local Expertise Marketplace Smart Contract
;; Connect community members with specific knowledge

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-EXPERT-NOT-FOUND (err u101))
(define-constant ERR-SESSION-NOT-FOUND (err u102))
(define-constant ERR-INVALID-AMOUNT (err u103))
(define-constant ERR-SESSION-ALREADY-COMPLETED (err u104))
(define-constant ERR-SESSION-NOT-ACTIVE (err u105))
(define-constant ERR-INSUFFICIENT-FUNDS (err u106))
(define-constant ERR-EXPERT-ALREADY-EXISTS (err u107))
(define-constant ERR-INVALID-RATING (err u108))
(define-constant ERR-CANNOT-RATE-SELF (err u109))

;; Data Variables
(define-data-var next-expert-id uint u1)
(define-data-var next-session-id uint u1)
(define-data-var platform-fee-percentage uint u5) ;; 5% platform fee

;; Data Maps
(define-map experts
    { expert-id: uint }
    {
        wallet: principal,
        name: (string-ascii 50),
        expertise: (string-ascii 100),
        hourly-rate: uint,
        total-sessions: uint,
        total-earnings: uint,
        average-rating: uint,
        rating-count: uint,
        is-active: bool
    }
)

(define-map expert-by-wallet
    { wallet: principal }
    { expert-id: uint }
)

(define-map sessions
    { session-id: uint }
    {
        expert-id: uint,
        client: principal,
        duration-hours: uint,
        total-cost: uint,
        status: (string-ascii 20), ;; "pending", "active", "completed", "cancelled"
        created-at: uint,
        completed-at: (optional uint),
        client-rating: (optional uint),
        expert-rating: (optional uint)
    }
)

(define-map session-escrow
    { session-id: uint }
    { amount: uint }
)

(define-map user-ratings
    { rater: principal, rated: principal, session-id: uint }
    { rating: uint, timestamp: uint }
)

;; Read-only functions
(define-read-only (get-expert (expert-id uint))
    (map-get? experts { expert-id: expert-id })
)

(define-read-only (get-expert-by-wallet (wallet principal))
    (match (map-get? expert-by-wallet { wallet: wallet })
        expert-data (get-expert (get expert-id expert-data))
        none
    )
)

(define-read-only (get-session (session-id uint))
    (map-get? sessions { session-id: session-id })
)

(define-read-only (get-session-escrow (session-id uint))
    (map-get? session-escrow { session-id: session-id })
)

(define-read-only (get-platform-fee-percentage)
    (var-get platform-fee-percentage)
)

(define-read-only (calculate-platform-fee (amount uint))
    (/ (* amount (var-get platform-fee-percentage)) u100)
)

(define-read-only (get-next-expert-id)
    (var-get next-expert-id)
)

(define-read-only (get-next-session-id)
    (var-get next-session-id)
)

;; Public functions
(define-public (register-expert (name (string-ascii 50)) (expertise (string-ascii 100)) (hourly-rate uint))
    (let
        (
            (expert-id (var-get next-expert-id))
            (existing-expert (map-get? expert-by-wallet { wallet: tx-sender }))
        )
        (asserts! (is-none existing-expert) ERR-EXPERT-ALREADY-EXISTS)
        (asserts! (> hourly-rate u0) ERR-INVALID-AMOUNT)
        
        (map-set experts
            { expert-id: expert-id }
            {
                wallet: tx-sender,
                name: name,
                expertise: expertise,
                hourly-rate: hourly-rate,
                total-sessions: u0,
                total-earnings: u0,
                average-rating: u0,
                rating-count: u0,
                is-active: true
            }
        )
        
        (map-set expert-by-wallet
            { wallet: tx-sender }
            { expert-id: expert-id }
        )
        
        (var-set next-expert-id (+ expert-id u1))
        (ok expert-id)
    )
)

(define-public (update-expert-profile (name (string-ascii 50)) (expertise (string-ascii 100)) (hourly-rate uint))
    (let
        (
            (expert-data (unwrap! (get-expert-by-wallet tx-sender) ERR-EXPERT-NOT-FOUND))
            (expert-id (get expert-id (unwrap! (map-get? expert-by-wallet { wallet: tx-sender }) ERR-EXPERT-NOT-FOUND)))
        )
        (asserts! (> hourly-rate u0) ERR-INVALID-AMOUNT)
        
        (map-set experts
            { expert-id: expert-id }
            (merge expert-data {
                name: name,
                expertise: expertise,
                hourly-rate: hourly-rate
            })
        )
        (ok true)
    )
)

(define-public (toggle-expert-status)
    (let
        (
            (expert-data (unwrap! (get-expert-by-wallet tx-sender) ERR-EXPERT-NOT-FOUND))
            (expert-id (get expert-id (unwrap! (map-get? expert-by-wallet { wallet: tx-sender }) ERR-EXPERT-NOT-FOUND)))
        )
        (map-set experts
            { expert-id: expert-id }
            (merge expert-data {
                is-active: (not (get is-active expert-data))
            })
        )
        (ok (not (get is-active expert-data)))
    )
)

(define-public (book-session (expert-id uint) (duration-hours uint))
    (let
        (
            (expert-data (unwrap! (get-expert expert-id) ERR-EXPERT-NOT-FOUND))
            (session-id (var-get next-session-id))
            (total-cost (* (get hourly-rate expert-data) duration-hours))
        )
        (asserts! (get is-active expert-data) ERR-NOT-AUTHORIZED)
        (asserts! (> duration-hours u0) ERR-INVALID-AMOUNT)
        (asserts! (not (is-eq tx-sender (get wallet expert-data))) ERR-CANNOT-RATE-SELF)
        
        ;; Transfer funds to escrow
        (try! (stx-transfer? total-cost tx-sender (as-contract tx-sender)))
        
        (map-set sessions
            { session-id: session-id }
            {
                expert-id: expert-id,
                client: tx-sender,
                duration-hours: duration-hours,
                total-cost: total-cost,
                status: "pending",
                created-at: block-height,
                completed-at: none,
                client-rating: none,
                expert-rating: none
            }
        )
        
        (map-set session-escrow
            { session-id: session-id }
            { amount: total-cost }
        )
        
        (var-set next-session-id (+ session-id u1))
        (ok session-id)
    )
)

(define-public (start-session (session-id uint))
    (let
        (
            (session-data (unwrap! (get-session session-id) ERR-SESSION-NOT-FOUND))
            (expert-data (unwrap! (get-expert (get expert-id session-data)) ERR-EXPERT-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get wallet expert-data)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status session-data) "pending") ERR-SESSION-NOT-ACTIVE)
        
        (map-set sessions
            { session-id: session-id }
            (merge session-data { status: "active" })
        )
        (ok true)
    )
)

(define-public (complete-session (session-id uint))
    (let
        (
            (session-data (unwrap! (get-session session-id) ERR-SESSION-NOT-FOUND))
            (expert-data (unwrap! (get-expert (get expert-id session-data)) ERR-EXPERT-NOT-FOUND))
            (escrow-data (unwrap! (get-session-escrow session-id) ERR-SESSION-NOT-FOUND))
            (platform-fee (calculate-platform-fee (get total-cost session-data)))
            (expert-payment (- (get total-cost session-data) platform-fee))
        )
        (asserts! (is-eq tx-sender (get wallet expert-data)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status session-data) "active") ERR-SESSION-NOT-ACTIVE)
        
        ;; Transfer payment to expert
        (try! (as-contract (stx-transfer? expert-payment tx-sender (get wallet expert-data))))
        
        ;; Transfer platform fee to contract owner
        (try! (as-contract (stx-transfer? platform-fee tx-sender CONTRACT-OWNER)))
        
        ;; Update session
        (map-set sessions
            { session-id: session-id }
            (merge session-data {
                status: "completed",
                completed-at: (some block-height)
            })
        )
        
        ;; Update expert stats
        (map-set experts
            { expert-id: (get expert-id session-data) }
            (merge expert-data {
                total-sessions: (+ (get total-sessions expert-data) u1),
                total-earnings: (+ (get total-earnings expert-data) expert-payment)
            })
        )
        
        ;; Remove from escrow
        (map-delete session-escrow { session-id: session-id })
        
        (ok true)
    )
)

(define-public (cancel-session (session-id uint))
    (let
        (
            (session-data (unwrap! (get-session session-id) ERR-SESSION-NOT-FOUND))
            (escrow-data (unwrap! (get-session-escrow session-id) ERR-SESSION-NOT-FOUND))
        )
        (asserts! (or (is-eq tx-sender (get client session-data))
                     (is-eq tx-sender CONTRACT-OWNER)) ERR-NOT-AUTHORIZED)
        (asserts! (not (is-eq (get status session-data) "completed")) ERR-SESSION-ALREADY-COMPLETED)
        
        ;; Refund client
        (try! (as-contract (stx-transfer? (get amount escrow-data) tx-sender (get client session-data))))
        
        ;; Update session
        (map-set sessions
            { session-id: session-id }
            (merge session-data { status: "cancelled" })
        )
        
        ;; Remove from escrow
        (map-delete session-escrow { session-id: session-id })
        
        (ok true)
    )
)

(define-public (rate-session (session-id uint) (rating uint) (is-client-rating bool))
    (let
        (
            (session-data (unwrap! (get-session session-id) ERR-SESSION-NOT-FOUND))
            (expert-data (unwrap! (get-expert (get expert-id session-data)) ERR-EXPERT-NOT-FOUND))
        )
        (asserts! (and (>= rating u1) (<= rating u5)) ERR-INVALID-RATING)
        (asserts! (is-eq (get status session-data) "completed") ERR-SESSION-NOT-ACTIVE)
        
        (if is-client-rating
            (begin
                (asserts! (is-eq tx-sender (get client session-data)) ERR-NOT-AUTHORIZED)
                (asserts! (is-none (get client-rating session-data)) ERR-SESSION-ALREADY-COMPLETED)
                
                ;; Update session with client rating
                (map-set sessions
                    { session-id: session-id }
                    (merge session-data { client-rating: (some rating) })
                )
                
                ;; Record rating
                (map-set user-ratings
                    { rater: tx-sender, rated: (get wallet expert-data), session-id: session-id }
                    { rating: rating, timestamp: block-height }
                )
                
                ;; Update expert's average rating
                (let
                    (
                        (new-rating-count (+ (get rating-count expert-data) u1))
                        (total-rating-points (+ (* (get average-rating expert-data) (get rating-count expert-data)) rating))
                        (new-average (/ total-rating-points new-rating-count))
                    )
                    (map-set experts
                        { expert-id: (get expert-id session-data) }
                        (merge expert-data {
                            average-rating: new-average,
                            rating-count: new-rating-count
                        })
                    )
                )
            )
            (begin
                (asserts! (is-eq tx-sender (get wallet expert-data)) ERR-NOT-AUTHORIZED)
                (asserts! (is-none (get expert-rating session-data)) ERR-SESSION-ALREADY-COMPLETED)
                
                ;; Update session with expert rating
                (map-set sessions
                    { session-id: session-id }
                    (merge session-data { expert-rating: (some rating) })
                )
                
                ;; Record rating
                (map-set user-ratings
                    { rater: tx-sender, rated: (get client session-data), session-id: session-id }
                    { rating: rating, timestamp: block-height }
                )
            )
        )
        (ok true)
    )
)

;; Admin functions
(define-public (set-platform-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (<= new-fee u20) ERR-INVALID-AMOUNT) ;; Max 20% fee
        (var-set platform-fee-percentage new-fee)
        (ok true)
    )
)