# 🏦 Pension Management System

A fair and transparent retirement savings platform built on Stacks blockchain with BTC staking capabilities and automated payouts.

## 🌟 Features

- 💰 **Secure Pension Accounts**: Create and manage your retirement savings
- 🎯 **BTC Staking Pools**: Earn rewards by staking your pension funds
- 🔄 **Automated Rewards**: Claim staking rewards automatically
- 👴 **Retirement Management**: Withdraw funds when you reach retirement age
- 🚨 **Emergency Withdrawals**: Access funds early with penalty (20%)
- 📊 **Transparent Tracking**: Monitor contributions, rewards, and balances

## 🚀 Getting Started

### Prerequisites

- Clarinet CLI installed
- Stacks wallet with STX tokens

### Installation

```bash
clarinet new pension-project
cd pension-project
```

Copy the contract code to `contracts/pension-management.clar`

## 📋 Usage Instructions

### 1. Create Pension Account 👤

```clarity
(contract-call? .pension-management create-pension-account u30)
```
*Creates account for 30-year-old user*

### 2. Make Contributions 💵

```clarity
(contract-call? .pension-management contribute u1000000)
```
*Contributes 1 STX to pension account*

### 3. Create Staking Pool 🏊‍♂️

```clarity
(contract-call? .pension-management create-staking-pool u500000)
```
*Stakes 0.5 STX in new pool*

### 4. Join Existing Pool 🤝

```clarity
(contract-call? .pension-management join-staking-pool u1 u300000)
```
*Joins pool #1 with 0.3 STX*

### 5. Claim Rewards 🎁

```clarity
(contract-call? .pension-management claim-staking-rewards u1)
```
*Claims rewards from pool #1*

### 6. Retire 🎉

```clarity
(contract-call? .pension-management retire)
```
*Activates retirement status (age 65+)*

### 7. Withdraw Pension 💸

```clarity
(contract-call? .pension-management withdraw-pension u500000)
```
*Withdraws 0.5 STX from pension (retirees only)*

## 🔍 Read-Only Functions

### Check Account Info 📊
```clarity
(contract-call? .pension-management get-account-info 'SP1ABC...)
```

### View Pool Information 🏊‍♀️
```clarity
(contract-call? .pension-management get-staking-pool-info u1)
```

### Calculate Current Age 📅
```clarity
(contract-call? .pension-management calculate-current-age 'SP1ABC...)
```

### Check Retirement Eligibility ✅
```clarity
(contract-call? .pension-management is-eligible-for-retirement 'SP1ABC...)
```

## ⚙️ Configuration

- **Retirement Age**: 65 years
- **Minimum Staking Period**: 52,560 blocks (~1 year
