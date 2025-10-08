# COBOL-to-Python Migration Validation Exercise

## Overview

This project demonstrates a **systematic approach to validating AI-generated code migrations** from legacy COBOL to modern Python, ensuring 100% behavioral equivalence.

**Original System**: User Management System (1987, COBOL, IBM-370 mainframe)
**Migrated System**: User Management System (2025, Python 3)

---

## What's Inside

### Core Files

| File | Purpose | Lines |
|------|---------|-------|
| [legacy_app.cob](legacy_app.cob) | Original COBOL source (1987) | 220 |
| [modern_app.py](modern_app.py) | Python implementation (AI-migrated) | 330 |
| [behavioral_spec.md](behavioral_spec.md) | Behavioral requirements specification | - |
| [behavior_tests.py](behavior_tests.py) | Black-box test suite (27 tests) | 320 |
| [evaluate_refactor.py](evaluate_refactor.py) | Automated grading system | 280 |
| [run_evaluation.py](run_evaluation.py) | Master evaluation runner | 115 |

### Documentation

| File | Purpose |
|------|---------|
| [METHODOLOGY.md](METHODOLOGY.md) | Complete methodology explanation |
| [CLAUDE_CODE_USAGE.md](CLAUDE_CODE_USAGE.md) | How Claude Code was used |
| **README.md** (this file) | Project overview and quickstart |

---

## Quick Start

### Prerequisites

- Python 3.7+ (tested on Python 3.13)
- No external dependencies (uses stdlib only)

### Run the Evaluation

```bash
# Clone or download this directory
cd cobol-exercise

# Run complete evaluation
python run_evaluation.py
```

**Expected Output:**
```
======================================================================
          COBOL-TO-PYTHON MIGRATION EVALUATION
               Complete Test & Grading Suite
======================================================================

Step 1: Checking dependencies...
OK: All required files found

Step 2: Running evaluation...

... (27 tests run) ...

======================================================================
FINAL SUMMARY
======================================================================

Tests Run:    27
Passed:       27
Failed:       0
Errors:       0
Pass Rate:    100.0%
Grade:        A - Excellent

SUCCESS: MIGRATION PASSED!

The Python implementation perfectly preserves all COBOL behaviors.
All behavioral tests passed.

======================================================================
```

---

## The Exercise

### Problem Statement

**Challenge**: Build a system that tests if AI correctly migrated COBOL to Python.

**Constraints**:
1. Tests must validate ANY Python implementation (not just one)
2. Tests must prove behavioral equivalence (not code similarity)
3. Tests must be comprehensive (cover all edge cases)

### Solution Approach

1. **Understand COBOL** ([legacy_app.cob](legacy_app.cob))
   - Analyze 1987 code line by line
   - Extract business logic, data structures, error conditions

2. **Define Correctness** ([behavioral_spec.md](behavioral_spec.md))
   - Document exact behaviors that MUST be preserved
   - Specify 25+ edge cases with expected outputs

3. **Build Tests** ([behavior_tests.py](behavior_tests.py))
   - Create 27 black-box behavioral tests
   - Cover all operations, edge cases, error paths

4. **Create Grading System** ([evaluate_refactor.py](evaluate_refactor.py))
   - Automate test execution
   - Generate quality scores (A-F)
   - Produce detailed reports

5. **Validate with Reference** ([modern_app.py](modern_app.py))
   - Migrate COBOL to Python
   - Prove 100% test pass rate is achievable

---

## What the System Does

### COBOL Application (1987)

**User Management System** with three operations:

1. **REGISTER-USER**: Create new user account
   - Max 100 users
   - Checks for duplicates
   - 20-character username/password

2. **LOGIN-USER**: Authenticate and create session
   - Validates credentials
   - Generates 6-digit session token
   - Max 50 concurrent sessions

3. **CHANGE-PASSWORD**: Update password with session token
   - Validates session token
   - Verifies old password
   - Updates to new password

### Python Implementation (2025)

**Same functionality**, modernized:
- Object-oriented design (`UserManagementSystem` class)
- Type hints for clarity
- Pythonic naming conventions
- **Exact behavioral equivalence** to COBOL

---

## Test Coverage

### Required Scenarios (12 Core Tests)

✅ Register user successfully
✅ Reject duplicate username
✅ Reject when DB full (100 users)
✅ Login with valid credentials
✅ Reject invalid credentials
✅ Generate valid session token
✅ Token maps to correct user
✅ Change password (valid token + old pass)
✅ Reject invalid token
✅ Reject wrong old password
✅ Handle multiple sessions
✅ Reject when session table full (50)

### Extended Coverage (15 Additional Tests)

- Case-sensitive username matching
- Duplicate check priority over capacity
- Empty username/password handling
- String truncation (20-char limit)
- Token = 0 validation
- Multiple password changes per session
- Password change persistence
- End-to-end workflows

**Total: 27 comprehensive tests**

---

## How to Use This Exercise

### Scenario 1: Validate Your Own Migration

1. Create your own `modern_app.py` with a `UserManagementSystem` class
2. Implement these methods:
   ```python
   def register_user(username: str, password: str) -> str
   def login_user(username: str, password: str) -> Tuple[str, Optional[int]]
   def change_password(token: int, old_pass: str, new_pass: str) -> str
   ```
3. Run `python run_evaluation.py`
4. Check results (aim for 100% pass rate)

### Scenario 2: Compare Different AI Models

1. Generate migrations with different AI models (GPT-4, Claude, Gemini, etc.)
2. Save each as `modern_app_gpt4.py`, `modern_app_claude.py`, etc.
3. Run evaluation on each:
   ```bash
   cp modern_app_gpt4.py modern_app.py && python run_evaluation.py
   cp modern_app_claude.py modern_app.py && python run_evaluation.py
   ```
4. Compare pass rates and quality scores

### Scenario 3: Learn COBOL-to-Python Migration

1. Read [legacy_app.cob](legacy_app.cob) to understand COBOL syntax
2. Read [behavioral_spec.md](behavioral_spec.md) to understand requirements
3. Study [modern_app.py](modern_app.py) to see migration patterns
4. Read [METHODOLOGY.md](METHODOLOGY.md) to understand the approach

---

## Key Insights

### 1. Test-First Prevents Confirmation Bias

Writing tests BEFORE migration ensures you validate against COBOL behavior, not your own assumptions.

### 2. Edge Cases are Critical

60% of tests focus on edge cases (full DB, duplicate users, invalid tokens) because that's where bugs hide.

### 3. String Handling is Subtle

COBOL's `PIC X(20)` (fixed 20-char strings with space-padding) requires careful implementation in Python.

### 4. Error Messages Must Match Exactly

Behavioral equivalence requires character-for-character error message matching:
- ✅ `"ERROR: USERNAME ALREADY EXISTS!"`
- ❌ `"User already exists"`

### 5. Order of Operations Matters

Example: Duplicate check must run BEFORE capacity check (lines 116-121 before line 126 in COBOL).

---

## Project Structure

```
cobol-exercise/
├── README.md                    # This file (project overview)
├── legacy_app.cob               # Original COBOL (DO NOT MODIFY)
├── behavioral_spec.md           # Requirements specification
├── behavior_tests.py            # Black-box test suite (27 tests)
├── evaluate_refactor.py         # Grading system
├── modern_app.py                # Reference Python implementation
├── run_evaluation.py            # Master test runner
├── METHODOLOGY.md               # Detailed methodology
└── CLAUDE_CODE_USAGE.md         # Claude Code usage notes
```

---

## Grading System

### Quality Score Scale

| Grade | Pass Rate | Meaning |
|-------|-----------|---------|
| A | 90-100% | Excellent migration |
| B | 80-89% | Good migration with minor issues |
| C | 70-79% | Acceptable with notable gaps |
| D | 60-69% | Poor with significant issues |
| F | <60% | Failed migration |

### Pass/Fail Determination

**PASS**: 100% test pass rate (all 27 tests)
**FAIL**: Any test failure

---

## Results

### Reference Implementation

**File**: [modern_app.py](modern_app.py)
**Tests Run**: 27
**Passed**: 27
**Failed**: 0
**Pass Rate**: 100.0%
**Grade**: A - Excellent
**Status**: ✅ PASSED

---

## Requirements Met

### Behavioral Spec ([behavioral_spec.md](behavioral_spec.md))

✅ All operations documented (register, login, change password)
✅ Edge cases covered (duplicate user, full DB, invalid token)
✅ Expected behaviors defined (what MUST be preserved)

### Test Suite ([behavior_tests.py](behavior_tests.py))

✅ All 12 required test scenarios
✅ 15 additional edge case tests
✅ Black-box design (implementation-agnostic)
✅ Exact error message validation

### Grading System ([evaluate_refactor.py](evaluate_refactor.py))

✅ Imports ANY modern_app.py
✅ Runs complete test suite
✅ Calculates pass/fail counts
✅ Generates quality score (A-F)
✅ Produces detailed reports

### Proof of Correctness

✅ Reference implementation passes all tests
✅ 100% pass rate achieved
✅ All 12 required scenarios validated
✅ Grade A quality score

---

## Technical Details

### COBOL Features Preserved

| COBOL Feature | Python Equivalent |
|---------------|-------------------|
| `PIC X(20)` (20-char strings) | `_normalize_string()` with padding/truncation |
| `OCCURS 100 TIMES` (fixed arrays) | `list` with capacity checking |
| `PERFORM VARYING` (loops) | `for` loop with slice `[:count]` |
| `FUNCTION RANDOM` | `random.randint(100000, 999999)` |
| `MOVE 1 TO USER-ACTIVE` | `user["active"] = 1` |

### String Normalization

COBOL `PIC X(20)` behavior:
- Shorter strings: right-padded with spaces (`"alice"` → `"alice               "`)
- Longer strings: truncated to 20 chars (`"verylongusername12345"` → `"verylongusername1234"`)
- Exact length: used as-is

Python implementation:
```python
def _normalize_string(self, value: str) -> str:
    if len(value) < 20:
        return value + (" " * (20 - len(value)))
    elif len(value) > 20:
        return value[:20]
    else:
        return value
```

---

## FAQ

**Q: Can I modify the COBOL file?**
A: No, [legacy_app.cob](legacy_app.cob) is the reference. Only modify [modern_app.py](modern_app.py).

**Q: What Python version is required?**
A: Python 3.7+ (tested on 3.13). No external dependencies.

**Q: Can I use different data structures?**
A: Yes! Tests are black-box. Use lists, dicts, classes, dataclasses—whatever works.

**Q: Must error messages match exactly?**
A: Yes. Behavioral equivalence requires character-for-character matching.

**Q: What if I want to add features?**
A: Fine, but don't break existing behavior. All 27 tests must still pass.

**Q: Can I improve security (hash passwords)?**
A: Not for this exercise. We're preserving 1987 behavior (plaintext passwords).

---

## License

This is an educational exercise. Feel free to use, modify, and share.

---

## Contact

For questions or feedback about this exercise, see the methodology and usage documentation.

---

**Last Updated**: 2025-10-08
**Exercise Version**: 1.0
**Test Suite Version**: 27 tests
**COBOL Source**: legacy_app.cob (1987-03-15)
**Python Target**: 3.7+
