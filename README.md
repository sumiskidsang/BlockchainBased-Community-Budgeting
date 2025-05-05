# Community Budgeting Smart Contract

A blockchain-based community budgeting system that allows citizens to propose projects, vote on them, and administrators to allocate funds based on community decisions.

## Overview

This smart contract enables a transparent and democratic process for community budget allocation:

- Citizens can register and propose community projects
- Each citizen can vote for projects they support
- Administrators manage the budget and fund approved projects
- All transactions and decisions are recorded on the blockchain for transparency

## Contract Functions

### User Management

- `register-citizen`: Register as a citizen with voting rights
- `register-admin`: Register as an administrator (restricted to existing admins)

### Project Management

- `propose-project`: Submit a new project proposal with name, description, and requested amount
- `vote-for-project`: Vote for a specific project (one vote per citizen per project)
- `cancel-project`: Cancel a project (creator or admin only)
- `reactivate-project`: Reactivate a previously canceled project (admin only)

### Budget Management

- `set-budget`: Set the total available budget (admin only)
- `toggle-voting`: Open or close the voting period (admin only)
- `fund-project`: Allocate funds to a project (admin only, after voting is closed)

### Read-Only Functions

- `get-project`: View details of a specific project
- `get-project-vote`: Check if a user has voted for a specific project
- `get-user-role`: Get the role of a specific user
- `get-budget`: View the current available budget
- `is-voting-open`: Check if voting is currently open
- `get-project-count`: Get the total number of projects

## Usage Example

1. Initialize the contract with administrators
2. Set the total budget
3. Citizens register and propose projects
4. Citizens vote for projects during the voting period
5. Administrators close voting and fund projects based on votes
6. The process can be repeated for new budget cycles

## Error Codes

- `ERR_UNAUTHORIZED (u100)`: User doesn't have permission
- `ERR_ALREADY_VOTED (u101)`: User has already voted for this project
- `ERR_PROJECT_NOT_FOUND (u102)`: Project ID doesn't exist
- `ERR_PROJECT_ALREADY_EXISTS (u103)`: Project ID already exists
- `ERR_INSUFFICIENT_FUNDS (u104)`: Not enough budget to fund the project
- `ERR_PROJECT_INACTIVE (u105)`: Project is not active
- `ERR_VOTING_CLOSED (u106)`: Voting period is closed
- `ERR_VOTING_OPEN (u107)`: Voting period is still open
- `ERR_ALREADY_FUNDED (u108)`: Project has already been funded
- `ERR_INVALID_AMOUNT (u109)`: Invalid amount specified
```
