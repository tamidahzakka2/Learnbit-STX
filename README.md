# 🎓 Learnbit - On-Chain Course Marketplace

A decentralized learning platform built on Stacks blockchain where instructors can create courses, students can enroll and pay with STX, and earn verifiable NFT certificates upon completion.

## ✨ Features

- 📚 **Course Creation**: Instructors can create courses with custom pricing and duration
- 💰 **STX Payments**: Students pay course fees directly in STX tokens
- 🏆 **NFT Certificates**: Verifiable completion certificates as NFTs
- ⭐ **Course Reviews**: Students can rate and review completed courses
- 💸 **Revenue Sharing**: Automatic fee distribution between instructors and platform
- 🔒 **Secure Enrollment**: Blockchain-verified course enrollment and completion

## 🚀 Quick Start

### Prerequisites
- Clarinet CLI installed
- Stacks wallet with STX tokens

### Installation

1. Clone the repository
2. Navigate to project directory
3. Deploy the contract using Clarinet

```bash
clarinet deploy
```

## 📖 Usage Guide

### For Instructors 👨‍🏫

#### Create a Course
```clarity
(contract-call? .Learnbit create-course 
  "Blockchain Fundamentals" 
  "Learn the basics of blockchain technology" 
  u1000000 
  u144)
```

#### Deactivate Course
```clarity
(contract-call? .Learnbit deactivate-course u1)
```

### For Students 👨‍🎓

#### Enroll in Course
```clarity
(contract-call? .Learnbit enroll-in-course u1)
```

#### Complete Course
```clarity
(contract-call? .Learnbit complete-course u1)
```

#### Get Certificate NFT
```clarity
(contract-call? .Learnbit issue-certificate u1)
```

#### Add Review
```clarity
(contract-call? .Learnbit add-review u1 u5 "Excellent course!")
```

### Read-Only Functions 📊

#### Get Course Information
```clarity
(contract-call? .Learnbit get-course u1)
```

#### Check Enrollment Status
```clarity
(contract-call? .Learnbit is-enrolled u1 'SP1234...)
```

#### View Certificate Details
```clarity
(contract-call? .Learnbit get-certificate u1)
```

## 💡 How It Works

1. **Course Creation**: Instructors create courses with title, description, price, and duration
2. **Student Enrollment**: Students pay STX to enroll, funds are split between instructor (95%) and platform (5%)
3. **Course Completion**: After the specified duration, students can mark courses as completed
4. **Certificate Issuance**: Completed students can mint NFT certificates as proof of completion
5. **Reviews**: Students can leave ratings and reviews for completed courses

## 🏗️ Contract Architecture

### Data Structures
- **Courses**: Store course metadata, pricing, and instructor information
- **Enrollments**: Track student enrollment and completion status
- **Certificates**: NFT metadata linking certificates to courses and students
- **Reviews**: Student feedback and ratings

### Key Functions
- `create-course`: Create new course offerings
- `enroll-in-course`: Student enrollment with STX payment
- `complete-course`: Mark course completion after duration
- `issue-certificate`: Mint NFT certificate for completed courses
- `add-review`: Submit course reviews and ratings

## 🔐 Security Features

- ✅ Payment validation before enrollment
- ✅ Time-based course completion verification
- ✅ Duplicate enrollment prevention
- ✅ Instructor authorization for course management
- ✅ Certificate uniqueness enforcement

## 💰 Economics

- **Platform Fee**: 5% of course price
- **Instructor Revenue**: 95% of course price
- **Certificate NFTs**: Free to mint after course completion
- **Reviews**: Free for enrolled students

## 🛠️ Development

### Testing
```bash
clarinet test
```

### Console
```bash
clarinet console
```

## 📄 License

MIT License - Build the future of decentralized education! 🚀

---

*Made with ❤️ for the Stacks ecosystem*

