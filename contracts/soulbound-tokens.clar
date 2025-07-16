

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-token-owner (err u101))
(define-constant err-token-exists (err u102))
(define-constant err-token-not-found (err u103))
(define-constant err-transfer-not-allowed (err u104))
(define-constant err-invalid-recipient (err u105))
(define-constant err-same-principal (err u106))
(define-constant err-token-revoked (err u107))
(define-constant err-invalid-metadata (err u108))

(define-non-fungible-token soulbound-token uint)

(define-data-var last-token-id uint u0)

(define-map token-metadata
  uint
  {
    name: (string-ascii 64),
    description: (string-ascii 256),
    image: (string-ascii 256),
    issued-at: uint,
    issuer: principal,
    attributes: (string-ascii 512)
  }
)

(define-map token-status
  uint
  { active: bool }
)

(define-map user-tokens
  principal
  (list 50 uint)
)

(define-map token-counts
  principal
  uint
)

(define-read-only (get-last-token-id)
  (var-get last-token-id)
)

(define-read-only (get-token-uri (token-id uint))
  (ok none)
)

(define-read-only (get-owner (token-id uint))
  (ok (nft-get-owner? soulbound-token token-id))
)

(define-read-only (get-token-metadata (token-id uint))
  (map-get? token-metadata token-id)
)

(define-read-only (get-token-status (token-id uint))
  (default-to { active: false } (map-get? token-status token-id))
)

(define-read-only (is-token-active (token-id uint))
  (get active (get-token-status token-id))
)

(define-read-only (get-user-tokens (user principal))
  (default-to (list) (map-get? user-tokens user))
)

(define-read-only (get-user-token-count (user principal))
  (default-to u0 (map-get? token-counts user))
)

(define-read-only (is-token-owner (token-id uint) (user principal))
  (is-eq (some user) (nft-get-owner? soulbound-token token-id))
)

(define-read-only (get-contract-info)
  {
    total-tokens: (var-get last-token-id),
    contract-owner: contract-owner
  }
)

(define-read-only (get-active-tokens-for-user (user principal))
  (get-user-tokens user)
)

(define-private (add-token-to-user (user principal) (token-id uint))
  (let
    (
      (current-tokens (get-user-tokens user))
      (current-count (get-user-token-count user))
    )
    (begin
      (map-set user-tokens user (unwrap-panic (as-max-len? (append current-tokens token-id) u50)))
      (map-set token-counts user (+ current-count u1))
      true
    )
  )
)

(define-private (remove-token-from-user (user principal) (token-id uint))
  (let
    (
      (current-count (get-user-token-count user))
    )
    (begin
      (map-set token-counts user (- current-count u1))
      true
    )
  )
)

(define-private (validate-metadata 
  (name (string-ascii 64))
  (description (string-ascii 256))
  (image (string-ascii 256))
  (attributes (string-ascii 512))
)
  (and
    (> (len name) u0)
    (> (len description) u0)
    (> (len image) u0)
  )
)

(define-public (mint-token
  (recipient principal)
  (name (string-ascii 64))
  (description (string-ascii 256))
  (image (string-ascii 256))
  (attributes (string-ascii 512))
)
  (let
    (
      (token-id (+ (var-get last-token-id) u1))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (not (is-eq recipient contract-owner)) err-same-principal)
    (asserts! (validate-metadata name description image attributes) err-invalid-metadata)
    
    (try! (nft-mint? soulbound-token token-id recipient))
    
    (map-set token-metadata token-id {
      name: name,
      description: description,
      image: image,
      issued-at: stacks-block-height,
      issuer: tx-sender,
      attributes: attributes
    })
    
    (map-set token-status token-id { active: true })
    (add-token-to-user recipient token-id)
    (var-set last-token-id token-id)
    
    (print {
      event: "mint",
      token-id: token-id,
      recipient: recipient,
      name: name
    })
    
    (ok token-id)
  )
)

(define-public (revoke-token (token-id uint))
  (let
    (
      (token-owner (unwrap! (nft-get-owner? soulbound-token token-id) err-token-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-token-active token-id) err-token-revoked)
    
    (map-set token-status token-id { active: false })
    
    (print {
      event: "revoke",
      token-id: token-id,
      owner: token-owner
    })
    
    (ok true)
  )
)

(define-public (burn-token (token-id uint))
  (let
    (
      (token-owner (unwrap! (nft-get-owner? soulbound-token token-id) err-token-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    (try! (nft-burn? soulbound-token token-id token-owner))
    (map-delete token-metadata token-id)
    (map-delete token-status token-id)
    (remove-token-from-user token-owner token-id)
    
    (print {
      event: "burn",
      token-id: token-id,
      owner: token-owner
    })
    
    (ok true)
  )
)

(define-public (transfer (token-id uint) (sender principal) (recipient principal))
  (err err-transfer-not-allowed)
)

(define-public (batch-mint
  (recipients (list 20 { recipient: principal, name: (string-ascii 64), description: (string-ascii 256), image: (string-ascii 256), attributes: (string-ascii 512) }))
)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ok (map mint-single-batch recipients))
  )
)

(define-private (mint-single-batch (data { recipient: principal, name: (string-ascii 64), description: (string-ascii 256), image: (string-ascii 256), attributes: (string-ascii 512) }))
  (mint-token 
    (get recipient data)
    (get name data)
    (get description data)
    (get image data)
    (get attributes data)
  )
)

(define-public (update-token-metadata
  (token-id uint)
  (name (string-ascii 64))
  (description (string-ascii 256))
  (image (string-ascii 256))
  (attributes (string-ascii 512))
)
  (let
    (
      (current-metadata (unwrap! (get-token-metadata token-id) err-token-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-token-active token-id) err-token-revoked)
    (asserts! (validate-metadata name description image attributes) err-invalid-metadata)
    
    (map-set token-metadata token-id (merge current-metadata {
      name: name,
      description: description,
      image: image,
      attributes: attributes
    }))
    
    (print {
      event: "metadata-update",
      token-id: token-id
    })
    
    (ok true)
  )
)

(define-read-only (get-tokens-by-status (active bool))
  (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10)
)

(define-read-only (verify-identity (user principal) (expected-token-count uint))
  (is-eq (get-user-token-count user) expected-token-count)
)
