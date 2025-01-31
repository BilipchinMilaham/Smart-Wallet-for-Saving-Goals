# Smart Wallet for Saving Goals

A decentralized savings application built on Stacks blockchain that helps users achieve their financial goals through smart contract-enforced discipline.

## Features

- Create personalized savings goals with custom target amounts
- Track progress toward savings targets
- Enforce withdrawal restrictions until goals are met
- Simple and secure savings management

## Smart Contract Functions

### Public Functions

`create-goal (target uint) (name (string-ascii 50))`
- Creates a new savings goal for the caller
- Parameters:
  - `target`: Target amount to save (in microSTX)
  - `name`: Name of the savings goal (max 50 ASCII characters)

`deposit (amount uint)`
- Deposits funds toward the active savings goal
- Parameters:
  - `amount`: Amount to deposit (in microSTX)

`withdraw (amount uint)` 
- Withdraws funds if savings goal is reached
- Parameters:
  - `amount`: Amount to withdraw (in microSTX)
- Restrictions:
  - Only allows withdrawal when current amount >= target amount

### Read-Only Functions

`get-goal (owner principal)`
- Returns details of an owner's savings goal
- Parameters:
  - `owner`: Principal address of the goal owner

## Error Codes

- `u100`: Goal not reached - Withdrawal attempted before reaching target
- `u101`: Insufficient funds
- `u102`: No savings goal found for user

## Testing

The contract includes comprehensive test coverage validating:
- Goal creation
- Deposit functionality  
- Withdrawal restrictions
- Goal achievement rewards
- Data retrieval

Run tests with:
```bash
npm test
