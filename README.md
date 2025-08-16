# 📝 Authoria - Proof of Authorship Smart Contract

## 🌟 Overview

Authoria is a blockchain-based proof of authorship system built on Stacks that allows writers and content creators to timestamp their original work on-chain. By registering content hashes with immutable blockchain timestamps, creators can establish verifiable proof of when they created their work.

## ✨ Features

- 🔐 **Immutable Timestamps**: Register your work with blockchain-verified timestamps
- 🔍 **Plagiarism Detection**: Check if content has been previously registered
- 👤 **Author Verification**: Verify the original author of any registered work
- 📚 **Work Portfolio**: Track all works by a specific author
- 🔄 **Ownership Transfer**: Transfer authorship rights to another party
- ✏️ **Description Updates**: Update work descriptions while preserving original proof

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Basic understanding of Clarity smart contracts

### Installation

```bash
git clone <your-repo>
cd authoria
clarinet check
```

## 📖 Usage

### Register New Work

```clarity
(contract-call? .Authoria register-work 
  "My Novel Title" 
  0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef
  u"A compelling story about blockchain technology")
```

### Verify Authorship

```clarity
(contract-call? .Authoria verify-authorship u1 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
```

### Check for Plagiarism

```clarity
(contract-call? .Authoria check-plagiarism 
  0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef
  'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
```

### Get Work Details

```clarity
(contract-call? .Authoria get-work u1)
```

## 🔧 Core Functions

### Public Functions

- `register-work` - Register new original work with title, content hash, and description
- `transfer-authorship` - Transfer ownership to another principal
- `update-work-description` - Update work description (author only)

### Read-Only Functions

- `get-work` - Retrieve work details by ID
- `get-work-by-hash` - Find work by content hash
- `verify-authorship` - Check if a principal is the author
- `check-plagiarism` - Detect potential plagiarism
- `get-author-work-count` - Count works by author
- `get-total-works` - Get total registered works

## 🛡️ Security Features

- Content hash uniqueness enforcement
- Author-only modification permissions
- Immutable timestamp records
- Blockchain-verified proof of creation

## 📊 Data Structure

Each registered work contains:
- Author principal
- Title (max 100 ASCII characters)
- SHA-256 content hash (32 bytes)
- Block timestamp
- Block height
- Description (max 500 UTF-8 characters)

## 🎯 Use Cases

- 📚 **Authors**: Protect manuscripts and articles
- 🎵 **Musicians**: Timestamp lyrics and compositions  
- 🎨 **Artists**: Prove creation date of digital art
- 📰 **Journalists**: Establish publication priority
- 🔬 **Researchers**: Document research findings
- 💼 **Businesses**: Protect proprietary content

## 🤝 Contributing

1. Fork the repository
2. Create your feature branch
3. Commit your changes
4. Push to the branch
5. Create a Pull Request

## 📄 License

This project is open source and available under the MIT License.

## 🆘 Support

For questions and support, please open an issue in the GitHub repository.

---


