;; Decentralized Film Funding Contract
;; Crowdfund movies with revenue-sharing NFTs

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_INVALID_AMOUNT (err u102))
(define-constant ERR_FUNDING_ENDED (err u103))
(define-constant ERR_FUNDING_ACTIVE (err u104))
(define-constant ERR_GOAL_NOT_MET (err u105))
(define-constant ERR_ALREADY_CLAIMED (err u106))
(define-constant ERR_NO_REVENUE (err u107))

;; Data Variables
(define-data-var project-counter uint u0)
(define-data-var nft-counter uint u0)

;; Data Maps
(define-map projects uint {
    creator: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    funding-goal: uint,
    funding-raised: uint,
    deadline: uint,
    revenue-pool: uint,
    status: (string-ascii 20),
    nft-supply: uint
})

(define-map project-funders {project-id: uint, funder: principal} {
    amount-funded: uint,
    nft-tokens: uint,
    revenue-claimed: uint
})

(define-map nft-ownership uint {
    project-id: uint,
    owner: principal,
    revenue-share: uint,
    minted-at: uint
})

(define-map project-revenue-per-share uint uint)

;; NFT Trait Implementation
(define-non-fungible-token film-funding-nft uint)

;; Private Functions
(define-private (calculate-revenue-share (funding-amount uint) (total-funding uint) (nft-supply uint))
    (if (> total-funding u0)
        (/ (* funding-amount nft-supply) total-funding)
        u0))

(define-private (mint-revenue-nft (project-id uint) (recipient principal) (revenue-share uint))
    (let ((token-id (+ (var-get nft-counter) u1)))
        (var-set nft-counter token-id)
        (try! (nft-mint? film-funding-nft token-id recipient))
        (map-set nft-ownership token-id {
            project-id: project-id,
            owner: recipient,
            revenue-share: revenue-share,
            minted-at: block-height
        })
        (ok token-id)))

;; Public Functions

;; Create a new film project
(define-public (create-project (title (string-ascii 100)) 
                              (description (string-ascii 500)) 
                              (funding-goal uint) 
                              (duration-blocks uint)
                              (nft-supply uint))
    (let ((project-id (+ (var-get project-counter) u1))
          (deadline (+ block-height duration-blocks)))
        (asserts! (> funding-goal u0) ERR_INVALID_AMOUNT)
        (asserts! (> nft-supply u0) ERR_INVALID_AMOUNT)
        (var-set project-counter project-id)
        (map-set projects project-id {
            creator: tx-sender,
            title: title,
            description: description,
            funding-goal: funding-goal,
            funding-raised: u0,
            deadline: deadline,
            revenue-pool: u0,
            status: "active",
            nft-supply: nft-supply
        })
        (ok project-id)))

;; Fund a project and receive revenue-sharing NFT
(define-public (fund-project (project-id uint) (amount uint))
    (let ((project (unwrap! (map-get? projects project-id) ERR_NOT_FOUND))
          (current-funding (get funding-raised project))
          (nft-supply (get nft-supply project))
          (existing-funding (default-to {amount-funded: u0, nft-tokens: u0, revenue-claimed: u0}
                                      (map-get? project-funders {project-id: project-id, funder: tx-sender}))))
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        (asserts! (<= block-height (get deadline project)) ERR_FUNDING_ENDED)
        (asserts! (is-eq (get status project) "active") ERR_FUNDING_ENDED)
        
        ;; Transfer STX from sender to contract
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        
        ;; Calculate revenue share percentage
        (let ((new-total-funding (+ current-funding amount))
              (revenue-share (calculate-revenue-share amount new-total-funding nft-supply)))
            
            ;; Mint NFT for the funder
            (try! (mint-revenue-nft project-id tx-sender revenue-share))
            
            ;; Update project funding
            (map-set projects project-id (merge project {
                funding-raised: new-total-funding
            }))
            
            ;; Update funder record
            (map-set project-funders {project-id: project-id, funder: tx-sender} {
                amount-funded: (+ (get amount-funded existing-funding) amount),
                nft-tokens: (+ (get nft-tokens existing-funding) u1),
                revenue-claimed: (get revenue-claimed existing-funding)
            })
            
            (ok true))))

;; Finalize project funding
(define-public (finalize-project (project-id uint))
    (let ((project (unwrap! (map-get? projects project-id) ERR_NOT_FOUND)))
        (asserts! (is-eq tx-sender (get creator project)) ERR_UNAUTHORIZED)
        (asserts! (>= block-height (get deadline project)) ERR_FUNDING_ACTIVE)
        
        (if (>= (get funding-raised project) (get funding-goal project))
            ;; Goal met - mark as funded
            (begin
                (map-set projects project-id (merge project {status: "funded"}))
                (ok "funded"))
            ;; Goal not met - mark as failed
            (begin
                (map-set projects project-id (merge project {status: "failed"}))
                (ok "failed")))))

;; Add revenue to the project pool
(define-public (add-revenue (project-id uint) (revenue-amount uint))
    (let ((project (unwrap! (map-get? projects project-id) ERR_NOT_FOUND)))
        (asserts! (is-eq tx-sender (get creator project)) ERR_UNAUTHORIZED)
        (asserts! (is-eq (get status project) "funded") ERR_FUNDING_ACTIVE)
        (asserts! (> revenue-amount u0) ERR_INVALID_AMOUNT)
        
        ;; Transfer revenue to contract
        (try! (stx-transfer? revenue-amount tx-sender (as-contract tx-sender)))
        
        ;; Update project revenue pool
        (map-set projects project-id (merge project {
            revenue-pool: (+ (get revenue-pool project) revenue-amount)
        }))
        
        ;; Calculate revenue per share
        (let ((total-nft-supply (get nft-supply project)))
            (if (> total-nft-supply u0)
                (map-set project-revenue-per-share project-id 
                        (+ (default-to u0 (map-get? project-revenue-per-share project-id))
                           (/ revenue-amount total-nft-supply)))
                false))
        
        (ok true)))

;; Claim revenue share for NFT holders
(define-public (claim-revenue (project-id uint))
    (let ((project (unwrap! (map-get? projects project-id) ERR_NOT_FOUND))
          (funder-data (unwrap! (map-get? project-funders {project-id: project-id, funder: tx-sender}) ERR_NOT_FOUND))
          (revenue-per-share (default-to u0 (map-get? project-revenue-per-share project-id))))
        (asserts! (> revenue-per-share u0) ERR_NO_REVENUE)
        
        (let ((claimable-revenue (* (get nft-tokens funder-data) revenue-per-share))
              (already-claimed (get revenue-claimed funder-data))
              (new-claimable (- claimable-revenue already-claimed)))
            (asserts! (> new-claimable u0) ERR_ALREADY_CLAIMED)
            
            ;; Transfer revenue to NFT holder
            (try! (as-contract (stx-transfer? new-claimable tx-sender tx-sender)))
            
            ;; Update claimed amount
            (map-set project-funders {project-id: project-id, funder: tx-sender}
                    (merge funder-data {revenue-claimed: claimable-revenue}))
            
            (ok new-claimable))))

;; Refund if project failed
(define-public (claim-refund (project-id uint))
    (let ((project (unwrap! (map-get? projects project-id) ERR_NOT_FOUND))
          (funder-data (unwrap! (map-get? project-funders {project-id: project-id, funder: tx-sender}) ERR_NOT_FOUND)))
        (asserts! (is-eq (get status project) "failed") ERR_FUNDING_ACTIVE)
        (asserts! (> (get amount-funded funder-data) u0) ERR_INVALID_AMOUNT)
        
        (let ((refund-amount (get amount-funded funder-data)))
            ;; Transfer refund
            (try! (as-contract (stx-transfer? refund-amount tx-sender tx-sender)))
            
            ;; Clear funder data
            (map-delete project-funders {project-id: project-id, funder: tx-sender})
            
            (ok refund-amount))))

;; Read-only functions

;; Get project details
(define-read-only (get-project (project-id uint))
    (map-get? projects project-id))

;; Get funding details for a specific funder
(define-read-only (get-funding-details (project-id uint) (funder principal))
    (map-get? project-funders {project-id: project-id, funder: funder}))

;; Get NFT details
(define-read-only (get-nft-details (token-id uint))
    (map-get? nft-ownership token-id))

;; Get total projects created
(define-read-only (get-project-count)
    (var-get project-counter))

;; Get revenue per share for a project
(define-read-only (get-revenue-per-share (project-id uint))
    (default-to u0 (map-get? project-revenue-per-share project-id)))

;; Check if address owns NFT
(define-read-only (get-owner (token-id uint))
    (ok (nft-get-owner? film-funding-nft token-id)))

;; Get the last token ID
(define-read-only (get-last-token-id)
    (ok (var-get nft-counter)))

;; Get token URI (placeholder)
(define-read-only (get-token-uri (token-id uint))
    (ok none))

;; Transfer NFT
(define-public (transfer (token-id uint) (sender principal) (recipient principal))
    (begin
        (asserts! (is-eq tx-sender sender) ERR_UNAUTHORIZED)
        (let ((nft-data (unwrap! (map-get? nft-ownership token-id) ERR_NOT_FOUND)))
            ;; Update NFT ownership record
            (map-set nft-ownership token-id (merge nft-data {owner: recipient}))
            ;; Transfer the NFT
            (nft-transfer? film-funding-nft token-id sender recipient))))

;; Initialize contract
(begin
    (var-set project-counter u0)
    (var-set nft-counter u0))