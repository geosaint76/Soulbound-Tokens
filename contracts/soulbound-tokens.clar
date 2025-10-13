

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
(define-constant err-issuer-not-authorized (err u109))
(define-constant err-issuer-already-exists (err u110))
(define-constant err-issuer-not-found (err u111))
(define-constant err-cannot-remove-owner (err u112))
(define-constant err-token-expired (err u113))
(define-constant err-invalid-expiration (err u114))
(define-constant err-already-expired (err u115))
(define-constant err-category-not-found (err u116))
(define-constant err-category-already-exists (err u117))
(define-constant err-invalid-category (err u118))

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
    attributes: (string-ascii 512),
    expires-at: (optional uint),
    renewable: bool,
    category-id: (optional uint)
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

(define-map authorized-issuers
  principal
  {
    name: (string-ascii 64),
    description: (string-ascii 256),
    authorized-at: uint,
    active: bool
  }
)

(define-data-var issuer-count uint u1)

(define-map token-categories
  uint
  {
    name: (string-ascii 64),
    description: (string-ascii 256),
    icon: (string-ascii 256),
    created-at: uint,
    active: bool
  }
)

(define-map category-token-counts
  uint
  uint
)

(define-data-var last-category-id uint u0)

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

(define-read-only (is-authorized-issuer (issuer principal))
  (let
    (
      (issuer-info (map-get? authorized-issuers issuer))
    )
    (match issuer-info
      some-info (get active some-info)
      false
    )
  )
)

(define-read-only (get-issuer-info (issuer principal))
  (map-get? authorized-issuers issuer)
)

(define-read-only (get-issuer-count)
  (var-get issuer-count)
)

(define-private (validate-issuer-metadata (name (string-ascii 64)) (description (string-ascii 256)))
  (and
    (> (len name) u0)
    (> (len description) u0)
  )
)

(define-read-only (get-category (category-id uint))
  (map-get? token-categories category-id)
)

(define-read-only (get-category-token-count (category-id uint))
  (default-to u0 (map-get? category-token-counts category-id))
)

(define-read-only (get-last-category-id)
  (var-get last-category-id)
)

(define-read-only (is-category-active (category-id uint))
  (let
    (
      (category (get-category category-id))
    )
    (match category
      some-cat (get active some-cat)
      false
    )
  )
)

(define-private (validate-category-metadata (name (string-ascii 64)) (description (string-ascii 256)) (icon (string-ascii 256)))
  (and
    (> (len name) u0)
    (> (len description) u0)
    (> (len icon) u0)
  )
)

(define-private (increment-category-count (category-id uint))
  (let
    (
      (current-count (get-category-token-count category-id))
    )
    (map-set category-token-counts category-id (+ current-count u1))
    true
  )
)

(define-public (create-category
  (name (string-ascii 64))
  (description (string-ascii 256))
  (icon (string-ascii 256))
)
  (let
    (
      (category-id (+ (var-get last-category-id) u1))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (validate-category-metadata name description icon) err-invalid-category)
    
    (map-set token-categories category-id {
      name: name,
      description: description,
      icon: icon,
      created-at: stacks-block-height,
      active: true
    })
    
    (map-set category-token-counts category-id u0)
    (var-set last-category-id category-id)
    
    (print {
      event: "category-created",
      category-id: category-id,
      name: name
    })
    
    (ok category-id)
  )
)

(define-public (update-category
  (category-id uint)
  (name (string-ascii 64))
  (description (string-ascii 256))
  (icon (string-ascii 256))
)
  (let
    (
      (category (unwrap! (get-category category-id) err-category-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (validate-category-metadata name description icon) err-invalid-category)
    
    (map-set token-categories category-id (merge category {
      name: name,
      description: description,
      icon: icon
    }))
    
    (print {
      event: "category-updated",
      category-id: category-id
    })
    
    (ok true)
  )
)

(define-public (deactivate-category (category-id uint))
  (let
    (
      (category (unwrap! (get-category category-id) err-category-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    (map-set token-categories category-id (merge category { active: false }))
    
    (print {
      event: "category-deactivated",
      category-id: category-id
    })
    
    (ok true)
  )
)

(define-public (reactivate-category (category-id uint))
  (let
    (
      (category (unwrap! (get-category category-id) err-category-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    (map-set token-categories category-id (merge category { active: true }))
    
    (print {
      event: "category-reactivated",
      category-id: category-id
    })
    
    (ok true)
  )
)

(define-read-only (is-token-expired (token-id uint))
  (let
    (
      (metadata (get-token-metadata token-id))
    )
    (match metadata
      some-metadata 
        (match (get expires-at some-metadata)
          some-expiration (>= stacks-block-height some-expiration)
          false
        )
      false
    )
  )
)

(define-read-only (is-token-valid (token-id uint))
  (and
    (is-token-active token-id)
    (not (is-token-expired token-id))
  )
)

(define-read-only (get-token-expiration (token-id uint))
  (let
    (
      (metadata (get-token-metadata token-id))
    )
    (match metadata
      some-metadata (get expires-at some-metadata)
      none
    )
  )
)

(define-read-only (blocks-until-expiration (token-id uint))
  (let
    (
      (expiration (get-token-expiration token-id))
    )
    (match expiration
      some-exp 
        (if (> some-exp stacks-block-height)
          (some (- some-exp stacks-block-height))
          (some u0)
        )
      none
    )
  )
)

(define-read-only (get-valid-tokens-for-user (user principal))
  (filter is-token-valid (get-user-tokens user))
)

(define-private (validate-expiration (expires-at (optional uint)))
  (match expires-at
    some-expiration (> some-expiration stacks-block-height)
    true
  )
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

(define-public (authorize-issuer
  (issuer principal)
  (name (string-ascii 64))
  (description (string-ascii 256))
)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (not (is-some (map-get? authorized-issuers issuer))) err-issuer-already-exists)
    (asserts! (validate-issuer-metadata name description) err-invalid-metadata)
    
    (map-set authorized-issuers issuer {
      name: name,
      description: description,
      authorized-at: stacks-block-height,
      active: true
    })
    
    (var-set issuer-count (+ (var-get issuer-count) u1))
    
    (print {
      event: "issuer-authorized",
      issuer: issuer,
      name: name
    })
    
    (ok true)
  )
)

(define-public (deauthorize-issuer (issuer principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (not (is-eq issuer contract-owner)) err-cannot-remove-owner)
    (asserts! (is-some (map-get? authorized-issuers issuer)) err-issuer-not-found)
    
    (map-delete authorized-issuers issuer)
    (var-set issuer-count (- (var-get issuer-count) u1))
    
    (print {
      event: "issuer-deauthorized",
      issuer: issuer
    })
    
    (ok true)
  )
)

(define-public (suspend-issuer (issuer principal))
  (let
    (
      (issuer-info (unwrap! (map-get? authorized-issuers issuer) err-issuer-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (not (is-eq issuer contract-owner)) err-cannot-remove-owner)
    
    (map-set authorized-issuers issuer (merge issuer-info { active: false }))
    
    (print {
      event: "issuer-suspended",
      issuer: issuer
    })
    
    (ok true)
  )
)

(define-public (reactivate-issuer (issuer principal))
  (let
    (
      (issuer-info (unwrap! (map-get? authorized-issuers issuer) err-issuer-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    (map-set authorized-issuers issuer (merge issuer-info { active: true }))
    
    (print {
      event: "issuer-reactivated",
      issuer: issuer
    })
    
    (ok true)
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
    (asserts! (or (is-eq tx-sender contract-owner) (is-authorized-issuer tx-sender)) err-issuer-not-authorized)
    (asserts! (not (is-eq recipient contract-owner)) err-same-principal)
    (asserts! (validate-metadata name description image attributes) err-invalid-metadata)
    
    (try! (nft-mint? soulbound-token token-id recipient))
    
    (map-set token-metadata token-id {
      name: name,
      description: description,
      image: image,
      issued-at: stacks-block-height,
      issuer: tx-sender,
      attributes: attributes,
      expires-at: none,
      renewable: false,
      category-id: none
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

(define-public (mint-token-with-expiration
  (recipient principal)
  (name (string-ascii 64))
  (description (string-ascii 256))
  (image (string-ascii 256))
  (attributes (string-ascii 512))
  (expires-at uint)
  (renewable bool)
)
  (let
    (
      (token-id (+ (var-get last-token-id) u1))
    )
    (asserts! (or (is-eq tx-sender contract-owner) (is-authorized-issuer tx-sender)) err-issuer-not-authorized)
    (asserts! (not (is-eq recipient contract-owner)) err-same-principal)
    (asserts! (validate-metadata name description image attributes) err-invalid-metadata)
    (asserts! (validate-expiration (some expires-at)) err-invalid-expiration)
    
    (try! (nft-mint? soulbound-token token-id recipient))
    
    (map-set token-metadata token-id {
      name: name,
      description: description,
      image: image,
      issued-at: stacks-block-height,
      issuer: tx-sender,
      attributes: attributes,
      expires-at: (some expires-at),
      renewable: renewable,
      category-id: none
    })
    
    (map-set token-status token-id { active: true })
    (add-token-to-user recipient token-id)
    (var-set last-token-id token-id)
    
    (print {
      event: "mint-with-expiration",
      token-id: token-id,
      recipient: recipient,
      name: name,
      expires-at: expires-at,
      renewable: renewable
    })
    
    (ok token-id)
  )
)

(define-public (mint-token-in-category
  (recipient principal)
  (name (string-ascii 64))
  (description (string-ascii 256))
  (image (string-ascii 256))
  (attributes (string-ascii 512))
  (category-id uint)
)
  (let
    (
      (token-id (+ (var-get last-token-id) u1))
    )
    (asserts! (or (is-eq tx-sender contract-owner) (is-authorized-issuer tx-sender)) err-issuer-not-authorized)
    (asserts! (not (is-eq recipient contract-owner)) err-same-principal)
    (asserts! (validate-metadata name description image attributes) err-invalid-metadata)
    (asserts! (is-category-active category-id) err-category-not-found)
    
    (try! (nft-mint? soulbound-token token-id recipient))
    
    (map-set token-metadata token-id {
      name: name,
      description: description,
      image: image,
      issued-at: stacks-block-height,
      issuer: tx-sender,
      attributes: attributes,
      expires-at: none,
      renewable: false,
      category-id: (some category-id)
    })
    
    (map-set token-status token-id { active: true })
    (add-token-to-user recipient token-id)
    (increment-category-count category-id)
    (var-set last-token-id token-id)
    
    (print {
      event: "mint-in-category",
      token-id: token-id,
      recipient: recipient,
      category-id: category-id,
      name: name
    })
    
    (ok token-id)
  )
)

(define-public (assign-token-to-category (token-id uint) (category-id uint))
  (let
    (
      (metadata (unwrap! (get-token-metadata token-id) err-token-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-category-active category-id) err-category-not-found)
    (asserts! (is-token-active token-id) err-token-revoked)
    
    (map-set token-metadata token-id (merge metadata {
      category-id: (some category-id)
    }))
    
    (increment-category-count category-id)
    
    (print {
      event: "token-categorized",
      token-id: token-id,
      category-id: category-id
    })
    
    (ok true)
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
    (asserts! (or (is-eq tx-sender contract-owner) (is-authorized-issuer tx-sender)) err-issuer-not-authorized)
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

(define-public (extend-token-expiration (token-id uint) (new-expires-at uint))
  (let
    (
      (metadata (unwrap! (get-token-metadata token-id) err-token-not-found))
      (token-owner (unwrap! (nft-get-owner? soulbound-token token-id) err-token-not-found))
    )
    (asserts! (or (is-eq tx-sender contract-owner) (is-authorized-issuer tx-sender)) err-issuer-not-authorized)
    (asserts! (is-token-active token-id) err-token-revoked)
    (asserts! (get renewable metadata) err-invalid-expiration)
    (asserts! (validate-expiration (some new-expires-at)) err-invalid-expiration)
    
    (map-set token-metadata token-id (merge metadata {
      expires-at: (some new-expires-at)
    }))
    
    (print {
      event: "expiration-extended",
      token-id: token-id,
      new-expires-at: new-expires-at,
      owner: token-owner
    })
    
    (ok true)
  )
)

(define-public (renew-expired-token (token-id uint) (new-expires-at uint))
  (let
    (
      (metadata (unwrap! (get-token-metadata token-id) err-token-not-found))
      (token-owner (unwrap! (nft-get-owner? soulbound-token token-id) err-token-not-found))
    )
    (asserts! (or (is-eq tx-sender contract-owner) (is-authorized-issuer tx-sender)) err-issuer-not-authorized)
    (asserts! (is-token-active token-id) err-token-revoked)
    (asserts! (get renewable metadata) err-invalid-expiration)
    (asserts! (is-token-expired token-id) err-already-expired)
    (asserts! (validate-expiration (some new-expires-at)) err-invalid-expiration)
    
    (map-set token-metadata token-id (merge metadata {
      expires-at: (some new-expires-at)
    }))
    
    (print {
      event: "token-renewed",
      token-id: token-id,
      new-expires-at: new-expires-at,
      owner: token-owner
    })
    
    (ok true)
  )
)

(define-public (make-token-renewable (token-id uint))
  (let
    (
      (metadata (unwrap! (get-token-metadata token-id) err-token-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-token-active token-id) err-token-revoked)
    
    (map-set token-metadata token-id (merge metadata {
      renewable: true
    }))
    
    (print {
      event: "token-made-renewable",
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

(map-set authorized-issuers contract-owner {
  name: "Contract Owner",
  description: "Original contract deployer with full administrative privileges",
  authorized-at: stacks-block-height,
  active: true
})