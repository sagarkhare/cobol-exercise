"""
Evaluation and Grading System for COBOL-to-Python Migration

This module provides automated grading for ANY Python implementation of the
COBOL User Management System (legacy_app.cob).

Usage:
    python evaluate_refactor.py

Returns:
    - Pass/Fail count
    - Coverage metrics
    - Detailed test results
    - Overall migration quality score
"""

import unittest
import sys
import io
from typing import Dict, List, Tuple, Any
from datetime import datetime


class EvaluationResults:
    """Container for evaluation results and metrics."""

    def __init__(self):
        self.total_tests = 0
        self.passed_tests = 0
        self.failed_tests = 0
        self.error_tests = 0
        self.skipped_tests = 0
        self.failures_detail = []
        self.errors_detail = []
        self.execution_time = 0.0

    @property
    def pass_rate(self) -> float:
        """Calculate pass rate as percentage."""
        if self.total_tests == 0:
            return 0.0
        return (self.passed_tests / self.total_tests) * 100

    @property
    def quality_score(self) -> str:
        """
        Calculate quality score based on pass rate.

        Grading scale:
        - A (90-100%): Excellent migration
        - B (80-89%): Good migration with minor issues
        - C (70-79%): Acceptable migration with notable gaps
        - D (60-69%): Poor migration with significant issues
        - F (<60%): Failed migration
        """
        rate = self.pass_rate

        if rate >= 90:
            return "A - Excellent"
        elif rate >= 80:
            return "B - Good"
        elif rate >= 70:
            return "C - Acceptable"
        elif rate >= 60:
            return "D - Poor"
        else:
            return "F - Failed"

    @property
    def is_passing(self) -> bool:
        """Migration passes if ALL tests pass (100%)."""
        return self.pass_rate == 100.0


class MigrationEvaluator:
    """
    Automated grading system for COBOL-to-Python migrations.

    This evaluator:
    1. Imports the Python implementation (modern_app.py)
    2. Runs the comprehensive behavior test suite
    3. Analyzes results and generates quality metrics
    4. Produces a detailed evaluation report
    """

    def __init__(self, verbose: bool = True):
        self.verbose = verbose
        self.results = EvaluationResults()

    def evaluate(self) -> EvaluationResults:
        """
        Run full evaluation of the Python implementation.

        Returns:
            EvaluationResults object with complete metrics
        """
        if self.verbose:
            self._print_header()

        # Check if modern_app.py exists and is importable
        if not self._check_implementation_exists():
            return self.results

        # Run behavioral tests
        self._run_behavior_tests()

        # Generate report
        if self.verbose:
            self._print_report()

        return self.results

    def _check_implementation_exists(self) -> bool:
        """Verify that modern_app.py exists and can be imported."""
        try:
            import modern_app
            if self.verbose:
                print("OK: modern_app.py found and importable")
                print()
            return True
        except ImportError as e:
            if self.verbose:
                print("ERROR: Cannot import modern_app.py")
                print(f"  {str(e)}")
                print()
                print("  Make sure modern_app.py exists and implements:")
                print("    - UserManagementSystem class")
                print("    - register_user(username, password) method")
                print("    - login_user(username, password) method")
                print("    - change_password(token, old_pass, new_pass) method")
                print()
            return False

    def _run_behavior_tests(self):
        """Execute the behavioral test suite."""
        if self.verbose:
            print("=" * 70)
            print("RUNNING BEHAVIORAL TESTS")
            print("=" * 70)
            print()

        # Import test suite
        from behavior_tests import UserManagementBehaviorTests

        # Create test suite
        loader = unittest.TestLoader()
        suite = loader.loadTestsFromTestCase(UserManagementBehaviorTests)

        # Run tests with custom result collector
        runner = unittest.TextTestRunner(stream=sys.stdout, verbosity=2 if self.verbose else 0)
        start_time = datetime.now()
        result = runner.run(suite)
        end_time = datetime.now()

        # Collect results
        self.results.total_tests = result.testsRun
        self.results.passed_tests = (
            result.testsRun - len(result.failures) - len(result.errors) - len(result.skipped)
        )
        self.results.failed_tests = len(result.failures)
        self.results.error_tests = len(result.errors)
        self.results.skipped_tests = len(result.skipped)
        self.results.failures_detail = result.failures
        self.results.errors_detail = result.errors
        self.results.execution_time = (end_time - start_time).total_seconds()

    def _print_header(self):
        """Print evaluation header."""
        print()
        print("=" * 70)
        print(" " * 15 + "COBOL-TO-PYTHON MIGRATION EVALUATOR")
        print("=" * 70)
        print()
        print(f"Evaluation started: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        print()

    def _print_report(self):
        """Print detailed evaluation report."""
        print()
        print("=" * 70)
        print("EVALUATION REPORT")
        print("=" * 70)
        print()

        # Summary metrics
        print("SUMMARY METRICS:")
        print(f"  Total Tests:      {self.results.total_tests}")
        print(f"  Passed:           {self.results.passed_tests}")
        print(f"  Failed:           {self.results.failed_tests}")
        print(f"  Errors:           {self.results.error_tests}")
        print(f"  Skipped:          {self.results.skipped_tests}")
        print()
        print(f"  Pass Rate:        {self.results.pass_rate:.1f}%")
        print(f"  Quality Score:    {self.results.quality_score}")
        print(f"  Execution Time:   {self.results.execution_time:.2f}s")
        print()

        # Pass/Fail determination
        print("-" * 70)
        if self.results.is_passing:
            print("MIGRATION STATUS: PASSED")
            print()
            print("  All behavioral tests passed. The Python implementation")
            print("  correctly preserves COBOL behavior.")
        else:
            print("MIGRATION STATUS: FAILED")
            print()
            print(f"  {self.results.failed_tests + self.results.error_tests} test(s) failed.")
            print("  The Python implementation does NOT correctly preserve")
            print("  COBOL behavior. Review failures below.")
        print("-" * 70)
        print()

        # Failure details
        if self.results.failures_detail or self.results.errors_detail:
            print("FAILURE DETAILS:")
            print()

            for test, traceback in self.results.failures_detail:
                print(f"FAILED: {test}")
                print(f"  {traceback}")
                print()

            for test, traceback in self.results.errors_detail:
                print(f"ERROR: {test}")
                print(f"  {traceback}")
                print()

        # Required test coverage
        print("REQUIRED TEST COVERAGE:")
        required_tests = [
            ("Register user successfully", "test_register_user_success"),
            ("Reject duplicate username", "test_register_duplicate_username"),
            ("Reject when DB full (100 users)", "test_register_db_full"),
            ("Login with valid credentials", "test_login_valid_credentials"),
            ("Reject invalid credentials", "test_login_invalid_password"),
            ("Generate valid session token", "test_login_generates_valid_token"),
            ("Token maps to correct user", "test_login_token_maps_to_user"),
            ("Change password (valid token + old pass)", "test_change_password_valid_token_and_old_password"),
            ("Reject invalid token", "test_change_password_invalid_token"),
            ("Reject wrong old password", "test_change_password_wrong_old_password"),
            ("Handle multiple sessions", "test_login_multiple_sessions_same_user"),
            ("Reject when session table full (50)", "test_login_session_table_full"),
        ]

        for description, test_name in required_tests:
            # Check if test passed
            test_passed = True
            for failed_test, _ in self.results.failures_detail + self.results.errors_detail:
                if test_name in str(failed_test):
                    test_passed = False
                    break

            status = "[PASS]" if test_passed else "[FAIL]"
            print(f"  {status} {description}")

        print()
        print("=" * 70)
        print(f"Evaluation completed: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        print("=" * 70)
        print()

    def generate_json_report(self) -> Dict[str, Any]:
        """
        Generate machine-readable JSON report.

        Returns:
            Dictionary with evaluation results
        """
        return {
            "timestamp": datetime.now().isoformat(),
            "summary": {
                "total_tests": self.results.total_tests,
                "passed": self.results.passed_tests,
                "failed": self.results.failed_tests,
                "errors": self.results.error_tests,
                "skipped": self.results.skipped_tests,
                "pass_rate": self.results.pass_rate,
                "quality_score": self.results.quality_score,
                "is_passing": self.results.is_passing,
                "execution_time": self.results.execution_time,
            },
            "failures": [
                {
                    "test": str(test),
                    "traceback": traceback
                }
                for test, traceback in self.results.failures_detail
            ],
            "errors": [
                {
                    "test": str(test),
                    "traceback": traceback
                }
                for test, traceback in self.results.errors_detail
            ],
        }


def main():
    """Main entry point for evaluation script."""
    evaluator = MigrationEvaluator(verbose=True)
    results = evaluator.evaluate()

    # Exit with appropriate code
    # 0 = all tests passed
    # 1 = some tests failed
    sys.exit(0 if results.is_passing else 1)


if __name__ == "__main__":
    main()
