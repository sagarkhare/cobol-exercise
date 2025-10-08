"""
Behavioral Test Suite for User Management System
Tests ANY Python implementation against COBOL legacy_app.cob behavior

This suite validates that a migrated Python version preserves ALL behaviors
from the original COBOL system, including edge cases and error conditions.

Requirements:
- Tests must be black-box (no knowledge of implementation)
- Tests must validate exact error messages
- Tests must verify data constraints (100 users, 50 sessions)
- Tests must check string handling (20-char padding/truncation)
"""

import unittest
from typing import Any, Optional


class UserManagementBehaviorTests(unittest.TestCase):
    """
    Black-box behavioral tests for User Management System.

    These tests validate that ANY Python implementation matches the exact
    behavior of the COBOL legacy_app.cob system.
    """

    def setUp(self):
        """Initialize a fresh system instance before each test."""
        # Import the system under test
        # This allows testing ANY implementation that provides these methods
        from modern_app import UserManagementSystem
        self.system = UserManagementSystem()

    def tearDown(self):
        """Clean up after each test."""
        self.system = None

    # =========================================================================
    # REGISTRATION TESTS
    # =========================================================================

    def test_register_user_success(self):
        """Register user successfully"""
        result = self.system.register_user("alice", "password123")
        self.assertEqual(result, "SUCCESS: USER REGISTERED!")

    def test_register_duplicate_username(self):
        """Reject duplicate username"""
        self.system.register_user("bob", "pass1")
        result = self.system.register_user("bob", "pass2")
        self.assertEqual(result, "ERROR: USERNAME ALREADY EXISTS!")

    def test_register_case_sensitive_usernames(self):
        """Username comparison is case-sensitive (Bob != bob)"""
        result1 = self.system.register_user("charlie", "pass1")
        result2 = self.system.register_user("Charlie", "pass2")

        self.assertEqual(result1, "SUCCESS: USER REGISTERED!")
        self.assertEqual(result2, "SUCCESS: USER REGISTERED!")

    def test_register_db_full(self):
        """Reject when DB full (100 users)"""
        # Register 100 users
        for i in range(100):
            result = self.system.register_user(f"user{i:03d}", "password")
            self.assertEqual(result, "SUCCESS: USER REGISTERED!")

        # 101st user should fail
        result = self.system.register_user("user100", "password")
        self.assertEqual(result, "ERROR: USER DATABASE FULL!")

    def test_register_duplicate_priority_over_full_db(self):
        """Duplicate check runs BEFORE capacity check"""
        # Register 100 users
        for i in range(100):
            self.system.register_user(f"user{i:03d}", "password")

        # Try to register duplicate when DB is full
        # Should show duplicate error, not full DB error
        result = self.system.register_user("user001", "newpass")
        self.assertEqual(result, "ERROR: USERNAME ALREADY EXISTS!")

    def test_register_empty_username(self):
        """Empty username is valid (stored as 20 spaces)"""
        result = self.system.register_user("", "password")
        self.assertEqual(result, "SUCCESS: USER REGISTERED!")

    def test_register_username_truncation(self):
        """Username longer than 20 chars is truncated"""
        long_username = "a" * 25  # 25 characters
        result = self.system.register_user(long_username, "password")
        self.assertEqual(result, "SUCCESS: USER REGISTERED!")

        # Verify truncation: registering same 20-char prefix should fail
        truncated_username = "a" * 20
        result2 = self.system.register_user(truncated_username, "password")
        self.assertEqual(result2, "ERROR: USERNAME ALREADY EXISTS!")

    # =========================================================================
    # LOGIN TESTS
    # =========================================================================

    def test_login_valid_credentials(self):
        """Login with valid credentials"""
        self.system.register_user("dave", "secret123")
        result, token = self.system.login_user("dave", "secret123")

        self.assertEqual(result, "SUCCESS: LOGIN APPROVED")
        self.assertIsNotNone(token)
        self.assertGreaterEqual(token, 100000)
        self.assertLessEqual(token, 999999)

    def test_login_invalid_username(self):
        """Reject invalid credentials (non-existent user)"""
        result, token = self.system.login_user("nonexistent", "password")

        self.assertEqual(result, "ERROR: INVALID CREDENTIALS!")
        self.assertIsNone(token)

    def test_login_invalid_password(self):
        """Reject invalid credentials (wrong password)"""
        self.system.register_user("eve", "correctpass")
        result, token = self.system.login_user("eve", "wrongpass")

        self.assertEqual(result, "ERROR: INVALID CREDENTIALS!")
        self.assertIsNone(token)

    def test_login_generates_valid_token(self):
        """Generate valid session token (6 digits: 100000-999999)"""
        self.system.register_user("frank", "password")
        result, token = self.system.login_user("frank", "password")

        self.assertEqual(result, "SUCCESS: LOGIN APPROVED")
        self.assertIsInstance(token, int)
        self.assertGreaterEqual(token, 100000)
        self.assertLessEqual(token, 999999)

    def test_login_token_maps_to_user(self):
        """Token maps to correct user"""
        self.system.register_user("grace", "pass1")
        self.system.register_user("henry", "pass2")

        result1, token1 = self.system.login_user("grace", "pass1")
        result2, token2 = self.system.login_user("henry", "pass2")

        # Both should succeed
        self.assertEqual(result1, "SUCCESS: LOGIN APPROVED")
        self.assertEqual(result2, "SUCCESS: LOGIN APPROVED")

        # Tokens should be different
        self.assertNotEqual(token1, token2)

    def test_login_multiple_sessions_same_user(self):
        """Handle multiple sessions (same user can login twice)"""
        self.system.register_user("iris", "password")

        result1, token1 = self.system.login_user("iris", "password")
        result2, token2 = self.system.login_user("iris", "password")

        self.assertEqual(result1, "SUCCESS: LOGIN APPROVED")
        self.assertEqual(result2, "SUCCESS: LOGIN APPROVED")

        # Different tokens for different sessions
        self.assertNotEqual(token1, token2)

    def test_login_session_table_full(self):
        """Reject when session table full (50 sessions)"""
        # Register 50 users
        for i in range(50):
            self.system.register_user(f"user{i:03d}", "password")

        # Create 50 sessions
        for i in range(50):
            result, token = self.system.login_user(f"user{i:03d}", "password")
            self.assertEqual(result, "SUCCESS: LOGIN APPROVED")

        # Register one more user and try to login (51st session)
        self.system.register_user("extrauser", "password")
        result, token = self.system.login_user("extrauser", "password")

        self.assertEqual(result, "ERROR: SESSION TABLE FULL!")
        self.assertIsNone(token)

    # =========================================================================
    # PASSWORD CHANGE TESTS
    # =========================================================================

    def test_change_password_valid_token_and_old_password(self):
        """Change password with valid token + old password"""
        self.system.register_user("jane", "oldpass")
        result, token = self.system.login_user("jane", "oldpass")

        result = self.system.change_password(token, "oldpass", "newpass")
        self.assertEqual(result, "SUCCESS: PASSWORD CHANGED!")

    def test_change_password_invalid_token(self):
        """Reject invalid token"""
        result = self.system.change_password(999999, "oldpass", "newpass")
        self.assertEqual(result, "ERROR: INVALID SESSION TOKEN!")

    def test_change_password_wrong_old_password(self):
        """Reject wrong old password"""
        self.system.register_user("kyle", "correctpass")
        result, token = self.system.login_user("kyle", "correctpass")

        result = self.system.change_password(token, "wrongpass", "newpass")
        self.assertEqual(result, "ERROR: OLD PASSWORD INCORRECT!")

    def test_change_password_then_login_with_new_password(self):
        """Password change persists (can login with new password)"""
        self.system.register_user("laura", "oldpass")
        result, token = self.system.login_user("laura", "oldpass")

        # Change password
        result = self.system.change_password(token, "oldpass", "newpass")
        self.assertEqual(result, "SUCCESS: PASSWORD CHANGED!")

        # Login with new password should succeed
        result, new_token = self.system.login_user("laura", "newpass")
        self.assertEqual(result, "SUCCESS: LOGIN APPROVED")
        self.assertIsNotNone(new_token)

    def test_change_password_then_login_with_old_password_fails(self):
        """Old password becomes invalid after change"""
        self.system.register_user("mike", "oldpass")
        result, token = self.system.login_user("mike", "oldpass")

        # Change password
        self.system.change_password(token, "oldpass", "newpass")

        # Login with old password should fail
        result, token = self.system.login_user("mike", "oldpass")
        self.assertEqual(result, "ERROR: INVALID CREDENTIALS!")
        self.assertIsNone(token)

    def test_change_password_token_zero(self):
        """Token = 0 is invalid"""
        result = self.system.change_password(0, "oldpass", "newpass")
        self.assertEqual(result, "ERROR: INVALID SESSION TOKEN!")

    def test_change_password_multiple_times_same_token(self):
        """Session token remains valid after password change (can reuse)"""
        self.system.register_user("nancy", "pass1")
        result, token = self.system.login_user("nancy", "pass1")

        # First change
        result1 = self.system.change_password(token, "pass1", "pass2")
        self.assertEqual(result1, "SUCCESS: PASSWORD CHANGED!")

        # Second change using same token
        result2 = self.system.change_password(token, "pass2", "pass3")
        self.assertEqual(result2, "SUCCESS: PASSWORD CHANGED!")

        # Verify final password is pass3
        result, new_token = self.system.login_user("nancy", "pass3")
        self.assertEqual(result, "SUCCESS: LOGIN APPROVED")

    # =========================================================================
    # STRING HANDLING TESTS
    # =========================================================================

    def test_username_exactly_20_chars(self):
        """Username exactly 20 characters is accepted"""
        username_20 = "a" * 20
        result = self.system.register_user(username_20, "password")
        self.assertEqual(result, "SUCCESS: USER REGISTERED!")

    def test_password_exactly_20_chars(self):
        """Password exactly 20 characters is accepted"""
        password_20 = "b" * 20
        result = self.system.register_user("oscar", password_20)
        self.assertEqual(result, "SUCCESS: USER REGISTERED!")

        # Verify can login with 20-char password
        result, token = self.system.login_user("oscar", password_20)
        self.assertEqual(result, "SUCCESS: LOGIN APPROVED")

    def test_password_truncation_21_chars(self):
        """Password longer than 20 chars is truncated"""
        password_25 = "c" * 25
        self.system.register_user("paul", password_25)

        # Login with truncated password (first 20 chars)
        password_20 = "c" * 20
        result, token = self.system.login_user("paul", password_20)
        self.assertEqual(result, "SUCCESS: LOGIN APPROVED")

    # =========================================================================
    # INTEGRATION TESTS
    # =========================================================================

    def test_full_workflow_register_login_change_password(self):
        """Complete workflow: register > login > change password > re-login"""
        # Register
        result = self.system.register_user("quinn", "initial_pass")
        self.assertEqual(result, "SUCCESS: USER REGISTERED!")

        # Login
        result, token = self.system.login_user("quinn", "initial_pass")
        self.assertEqual(result, "SUCCESS: LOGIN APPROVED")
        self.assertIsNotNone(token)

        # Change password
        result = self.system.change_password(token, "initial_pass", "new_pass")
        self.assertEqual(result, "SUCCESS: PASSWORD CHANGED!")

        # Re-login with new password
        result, new_token = self.system.login_user("quinn", "new_pass")
        self.assertEqual(result, "SUCCESS: LOGIN APPROVED")
        self.assertIsNotNone(new_token)

    def test_empty_password_registration_and_login(self):
        """Empty password is valid (stored as 20 spaces)"""
        result = self.system.register_user("rachel", "")
        self.assertEqual(result, "SUCCESS: USER REGISTERED!")

        # Login with empty password
        result, token = self.system.login_user("rachel", "")
        self.assertEqual(result, "SUCCESS: LOGIN APPROVED")

    def test_whitespace_password_change(self):
        """Password change to empty string is valid"""
        self.system.register_user("steve", "oldpass")
        result, token = self.system.login_user("steve", "oldpass")

        # Change to empty password
        result = self.system.change_password(token, "oldpass", "")
        self.assertEqual(result, "SUCCESS: PASSWORD CHANGED!")

        # Login with empty password
        result, new_token = self.system.login_user("steve", "")
        self.assertEqual(result, "SUCCESS: LOGIN APPROVED")


class TestCoverageReport(unittest.TestCase):
    """
    Meta-test to report coverage of required behavioral scenarios.
    """

    def test_coverage_checklist(self):
        """Verify all 12+ required scenarios are covered"""
        required_tests = [
            "test_register_user_success",
            "test_register_duplicate_username",
            "test_register_db_full",
            "test_login_valid_credentials",
            "test_login_invalid_password",
            "test_login_generates_valid_token",
            "test_login_token_maps_to_user",
            "test_change_password_valid_token_and_old_password",
            "test_change_password_invalid_token",
            "test_change_password_wrong_old_password",
            "test_login_multiple_sessions_same_user",
            "test_login_session_table_full",
        ]

        # Verify all required tests exist
        for test_name in required_tests:
            self.assertTrue(
                hasattr(UserManagementBehaviorTests, test_name),
                f"Required test '{test_name}' not found"
            )


def run_tests_with_report():
    """
    Run all tests and generate a detailed report.
    Returns: (total_tests, passed, failed, errors)
    """
    loader = unittest.TestLoader()
    suite = loader.loadTestsFromTestCase(UserManagementBehaviorTests)
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)

    total = result.testsRun
    passed = total - len(result.failures) - len(result.errors)
    failed = len(result.failures)
    errors = len(result.errors)

    return total, passed, failed, errors


if __name__ == "__main__":
    print("=" * 70)
    print("BEHAVIORAL TEST SUITE - User Management System")
    print("Testing Python implementation against COBOL legacy_app.cob behavior")
    print("=" * 70)
    print()

    # Run tests
    unittest.main(verbosity=2)
