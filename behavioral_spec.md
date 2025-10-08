# Behavioral Specification: User Management System

This document defines the **exact behaviors** that ANY Python migration of `legacy_app.cob` MUST preserve.

---

## System Constraints

| Constraint | Value | Behavior When Exceeded |
|------------|-------|------------------------|
| Maximum Users | 100 | Display "ERROR: USER DATABASE FULL!" |
| Maximum Sessions | 50 | Display "ERROR: SESSION TABLE FULL!" |
| Username Length | 20 characters | Input truncated/padded to 20 chars |
| Password Length | 20 characters | Input truncated/padded to 20 chars |
| Session Token Format | 6-digit number (100000-999999) | Generated via RANDOM function |

### Full Database Capacity Behavior

**User Database Full (USER-COUNT >= 100):**
- **Location in COBOL:** Lines 126-135
- **Check order:** Duplicate username check happens FIRST (lines 116-121), capacity check SECOND (line 126)
- **Critical edge case:** If DB has 100 users and you register a duplicate, error is "USERNAME ALREADY EXISTS!" not "DATABASE FULL!"
- **When triggered:** Attempting to register the 101st user (when USER-COUNT already = 100)
- **Exact message:** `"ERROR: USER DATABASE FULL!"`

**Session Table Full (SESSION-COUNT >= 50):**
- **Location in COBOL:** Lines 160-170
- **Check order:** Credentials validated FIRST, session token generated, THEN capacity checked
- **Critical edge case:** Valid login consumes a random token but fails if session table full
- **When triggered:** Attempting to create 51st session (when SESSION-COUNT already = 50)
- **Exact message:** `"ERROR: SESSION TABLE FULL!"`
- **Side effect:** Token generation happens before capacity check (token is wasted)

---

## Operation 1: REGISTER NEW USER

### Input
- Username (string, max 20 chars)
- Password (string, max 20 chars)

### Pre-conditions
- System initialized with user count = 0

### Success Path
1. Accept username input
2. Accept password input
3. Check if username already exists in user table
4. If username is unique AND user count < 100:
   - Increment user count by 1
   - Store username at position [user_count]
   - Store password at position [user_count]
   - Set user as active (flag = 1)
   - Display: `"SUCCESS: USER REGISTERED!"`

### Error Conditions

| Condition | Error Message |
|-----------|---------------|
| Username already exists | `"ERROR: USERNAME ALREADY EXISTS!"` |
| User count >= 100 | `"ERROR: USER DATABASE FULL!"` |

### Critical Behaviors
- **Username comparison is case-sensitive**
- **Duplicate check scans from index 1 to user_count** (not all 100 slots)
- **Only active users (USER-ACTIVE = 1) are stored**
- **Usernames with trailing spaces are treated as distinct** (e.g., "john" ≠ "john ")

---

## Operation 2: LOGIN USER

### Input
- Username (string, max 20 chars)
- Password (string, max 20 chars)

### Pre-conditions
- User must be registered and active

### Success Path
1. Accept username input
2. Accept password input
3. Search user table for matching username (index 1 to user_count)
4. If username found:
   - Verify password matches stored password
   - Verify user is active (USER-ACTIVE = 1)
5. If all checks pass:
   - Generate random 6-digit session token
   - If session count < 50:
     - Increment session count by 1
     - Store token at position [session_count]
     - Store username at position [session_count]
     - Set session as active (SESSION-ACTIVE = 1)
     - Display: `"SUCCESS: LOGIN APPROVED"`
     - Display: `"YOUR SESSION TOKEN: [6-digit-number]"`

### Error Conditions

| Condition | Error Message |
|-----------|---------------|
| Username not found | `"ERROR: INVALID CREDENTIALS!"` |
| Password incorrect | `"ERROR: INVALID CREDENTIALS!"` |
| User not active | `"ERROR: INVALID CREDENTIALS!"` |
| Session count >= 50 | `"ERROR: SESSION TABLE FULL!"` |

### Critical Behaviors
- **Invalid username and invalid password produce IDENTICAL error messages** (security through obscurity)
- **Session token is generated BEFORE checking session table capacity**
- **Token range: 100000 to 999999** (RANDOM * 900000 + 100000)
- **Multiple logins by same user create separate sessions** (no session reuse)

---

## Operation 3: CHANGE PASSWORD

### Input
- Session token (6-digit number)
- Old password (string, max 20 chars)
- New password (string, max 20 chars)

### Pre-conditions
- User must have active session (from prior login)

### Success Path
1. Accept session token input
2. Accept old password input
3. Accept new password input
4. Search session table for matching token (index 1 to session_count)
5. If token found AND session is active:
   - Retrieve username from session entry
6. Search user table for matching username (index 1 to user_count)
7. If user found:
   - Verify old password matches stored password
   - If match:
     - Update password to new password
     - Display: `"SUCCESS: PASSWORD CHANGED!"`
   - If no match:
     - Display: `"ERROR: OLD PASSWORD INCORRECT!"`

### Error Conditions

| Condition | Error Message |
|-----------|---------------|
| Token not found | `"ERROR: INVALID SESSION TOKEN!"` |
| Token exists but session inactive | `"ERROR: INVALID SESSION TOKEN!"` |
| Old password incorrect | `"ERROR: OLD PASSWORD INCORRECT!"` |

### Critical Behaviors
- **Session token must be exact numeric match** (189456 ≠ 189457)
- **Old password verification happens AFTER token validation**
- **Password change is permanent** (no rollback on error)
- **Session remains active after password change** (not invalidated)
- **WS-SUCCESS-FLAG prevents checking multiple users** (loop exits on first match)

---

## Data Structure Behaviors

### User Table
- **Fixed array of 100 entries** (indices 1-100)
- Each entry contains:
  - USER-NAME: 20-character string (space-padded)
  - USER-PASSWORD: 20-character string (space-padded)
  - USER-ACTIVE: Single digit (0 = inactive, 1 = active)

### Session Table
- **Fixed array of 50 entries** (indices 1-50)
- Each entry contains:
  - SESSION-TOKEN: 6-digit number
  - SESSION-USER: 20-character string (space-padded)
  - SESSION-ACTIVE: Single digit (0 = inactive, 1 = active)

### String Handling
- **All strings are exactly 20 characters** (COBOL PIC X(20))
- **Shorter inputs are right-padded with spaces** (e.g., "alice" → "alice               ")
- **Longer inputs are truncated** (input beyond 20 chars is ignored)
- **Comparison includes trailing spaces** ("alice" with 15 spaces is the stored value)

---

## Initialization Behavior

On system startup:
1. Set USER-COUNT = 0
2. Set SESSION-COUNT = 0
3. For each of 100 user entries:
   - Set USER-NAME = 20 spaces
   - Set USER-PASSWORD = 20 spaces
   - Set USER-ACTIVE = 0
4. For each of 50 session entries:
   - Set SESSION-TOKEN = 0
   - Set SESSION-USER = 20 spaces
   - Set SESSION-ACTIVE = 0

---

## Edge Cases That MUST Be Tested

### Registration Edge Cases
1. **Registering 100th user** → Should succeed with "SUCCESS: USER REGISTERED!"
2. **Registering 101st user** → Should fail with "ERROR: USER DATABASE FULL!"
3. **Duplicate username with different case** → Should succeed (case-sensitive)
4. **Duplicate username exact match** → Should fail with "ERROR: USERNAME ALREADY EXISTS!"
5. **Empty username** → Should register successfully (stored as 20 spaces)
6. **Duplicate check priority**: When DB is full (100 users) and username is duplicate → "ERROR: USERNAME ALREADY EXISTS!" (duplicate check runs BEFORE capacity check)
7. **Username exactly 20 characters** → Should accept fully (no truncation)
8. **Username 21+ characters** → Should truncate to 20 characters
9. **Empty password** → Should accept (stored as 20 spaces)

### Login Edge Cases
1. **Correct credentials** → Should create session and return token
2. **Correct username, wrong password** → "ERROR: INVALID CREDENTIALS!"
3. **Non-existent username** → "ERROR: INVALID CREDENTIALS!"
4. **50th session creation** → Should succeed with valid token
5. **51st session creation** → "ERROR: SESSION TABLE FULL!" (even with valid credentials)
6. **Same user logging in twice** → Should create 2 separate sessions with different tokens
7. **Session token range validation** → Token must be 100000-999999 (6 digits)
8. **Login after successful password change with OLD password** → "ERROR: INVALID CREDENTIALS!"

### Password Change Edge Cases
1. **Valid token + correct old password** → "SUCCESS: PASSWORD CHANGED!"
2. **Invalid token** → "ERROR: INVALID SESSION TOKEN!"
3. **Valid token + incorrect old password** → "ERROR: OLD PASSWORD INCORRECT!"
4. **Token = 0** → "ERROR: INVALID SESSION TOKEN!"
5. **Password change then login with new password** → Should succeed
6. **Password change then login with old password** → Should fail
7. **Multiple password changes using same token** → All should succeed (session not invalidated)
8. **Password change to empty string** → Should succeed (stored as 20 spaces)

---

## Non-Functional Requirements

### Security Considerations (Historical)
- **Passwords stored in plaintext** (no hashing - this is 1987)
- **Session tokens are predictable** (simple RANDOM function, not cryptographically secure)
- **No session expiration** (tokens valid indefinitely)
- **No session invalidation on password change**

### Performance Characteristics
- **Linear search** for all lookups (O(n) complexity)
- **No indexing or hash tables**
- **Sequential scanning from index 1 to count**

### Compatibility Requirements
- **Exact error message text** (including capitalization, punctuation)
- **Exact output format** for session tokens
- **Same behavior for whitespace in inputs**

---

## What a "Correct Migration" Means

A Python implementation is **behaviorally equivalent** if and only if:

1. ✅ All 12 test scenarios produce identical outputs
2. ✅ All error messages match character-for-character
3. ✅ Data capacity limits (100 users, 50 sessions) are enforced
4. ✅ String handling preserves 20-character padding/truncation
5. ✅ Session token generation produces 6-digit numbers in range [100000, 999999]
6. ✅ Duplicate detection uses case-sensitive exact matching
7. ✅ Search algorithms scan only populated entries (1 to count, not full array)
8. ✅ Initialization creates empty tables with correct defaults

**Implementation details that CAN differ:**
- Data structures (lists vs arrays vs dictionaries)
- Programming paradigms (OOP vs functional vs procedural)
- Internal variable names
- Code organization (single file vs modules)

**Behaviors that MUST NOT differ:**
- User-visible outputs (prompts, messages, token display)
- Error handling logic
- State transitions
- Data persistence within session
