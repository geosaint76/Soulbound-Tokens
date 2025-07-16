# 🔗 Soulbound Tokens Contract

## 🎯 Overview

A **Soulbound Tokens** smart contract implementation on the Stacks blockchain using Clarity. Soulbound tokens are non-transferable NFTs that represent identity, achievements, or credentials permanently bound to a user's wallet address.

## ✨ Features

- 🚫 **Non-transferable**: Tokens cannot be moved between wallets
- 👤 **Identity-bound**: Permanently linked to the recipient's address  
- 🎨 **Rich metadata**: Name, description, image, and custom attributes
- 🔄 **Status management**: Active/revoked token states
- 🧑‍💼 **Owner controls**: Mint, revoke, burn, and batch operations
- 📊 **User tracking**: View all tokens owned by an address
- 🔍 **Verification**: Identity verification through token ownership

## 🚀 Quick Start

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Node.js (for testing)

### Installation

```bash
git clone <your-repo>
cd soulbound-tokens
clarinet check
```

## 📋 Contract Functions

### 🔒 Owner Functions

#### `mint-token`
```clarity
(mint-token recipient name description image attributes)
```
Mint a new soulbound token to a recipient.

#### `revoke-token` 
```clarity
(revoke-token token-id)
```
Revoke a token (makes it inactive but doesn't delete it).

#### `burn-token`
```clarity
(burn-token token-id)
```
Permanently destroy a token and remove all data.

#### `batch-mint`
```clarity
(batch-mint recipients-list)
```
Mint multiple tokens in a single transaction (up to 20).

#### `update-token-metadata`
```clarity
(update-token-metadata token-id name description image attributes)
```
Update metadata for an existing active token.

### 📖 Read-Only Functions

#### `get-owner`
```clarity
(get-owner token-id)
```
Get the owner of a specific token.

#### `get-token-metadata`
```clarity
(get-token-metadata token-id)
```
Retrieve complete metadata for a token.

#### `get-user-tokens`
```clarity
(get-user-tokens user-principal)
```
Get all token IDs owned by a user.

#### `get-active-tokens-for-user`
```clarity
(get-active-tokens-for-user user-principal)
```
Get only active tokens for a user.

#### `is-token-active`
```clarity
(is-token-active token-id)
```
Check if a token is currently active.

#### `verify-identity`
```clarity
(verify-identity user expected-count)
```
Verify a user has an expected number of tokens.

## 🛠️ Usage Examples

### Testing with Clarinet Console

```clarity
;; Deploy contract
::get_contracts

;; Mint a token
(contract-call? .soulbound-tokens mint-token 
  'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM
  "Achievement Badge"
  "Completed blockchain development course"
  "https://example.com/badge.png"
  "course:blockchain,level:beginner,year:2024")

;; Check token owner
(contract-call? .soulbound-tokens get-owner u1)

;; Get user's tokens
(contract-call? .soulbound-tokens get-user-tokens 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)

;; Try to transfer (will fail)
(contract-call? .soulbound-tokens transfer u1 
  'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM 
  'ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG)
```

## 📊 Data Structures

### Token Metadata
```clarity
{
  name: (string-ascii 64),
  description: (string-ascii 256), 
  image: (string-ascii 256),
  issued-at: uint,
  issuer: principal,
  attributes: (string-ascii 512)
}
```

### Token Status
```clarity
{ active: bool }
```

## ⚠️ Error Codes

| Code | Description |
|------|-------------|
| u100 | Owner only |
| u101 | Not token owner |
| u102 | Token exists |
| u103 | Token not found |
| u104 | Transfer not allowed |
| u105 | Invalid recipient |
| u106 | Same principal |
| u107 | Token revoked |
| u108 | Invalid metadata |

## 🎨 Use Cases

- 🎓 **Academic Credentials**: Diplomas, certificates, course completions
- 🏆 **Achievement Badges**: Gaming achievements, skill certifications
- 🆔 **Identity Tokens**: KYC verification, membership status
- 🎖️ **Reputation Systems**: Community standing, trust scores
- 📋 **Compliance Certificates**: Regulatory approvals, safety training
- 🔐 **Access Control**: Permanent access rights, role assignments

## 🧪 Testing

Run the test suite:

```bash
clarinet test
```

## 📈 Contract Limits

- Maximum 50 tokens per user (adjustable)
- Batch mint up to 20 tokens
- String limits: name (64), description (256), image (256), attributes (512)

## 🔐 Security Considerations

- Only contract owner can mint, revoke, and burn tokens
- Transfers are permanently disabled
- Metadata validation prevents empty fields
- Token status tracking for lifecycle management

## 📄 License

MIT License - feel free to use this contract as a foundation for your soulbound token implementations!

## 🤝 Contributing

Contributions welcome! Please ensure all tests pass and follow the existing code style.

---

*Built with ❤️ on Stacks blockchain using Clarity smart contracts*
