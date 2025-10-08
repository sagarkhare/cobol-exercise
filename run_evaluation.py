"""
Master Evaluation Runner

This script runs the complete evaluation pipeline:
1. Validates modern_app.py exists
2. Runs all behavioral tests
3. Generates detailed evaluation report
4. Shows pass/fail summary

Usage:
    python run_evaluation.py

Exit codes:
    0 - All tests passed (100% migration success)
    1 - Some tests failed
    2 - Critical error (modern_app.py not found)
"""

import sys
import os
from pathlib import Path


def check_dependencies():
    """Verify all required files exist."""
    required_files = [
        "legacy_app.cob",
        "behavioral_spec.md",
        "behavior_tests.py",
        "evaluate_refactor.py",
        "modern_app.py",
    ]

    missing_files = []
    for filename in required_files:
        if not Path(filename).exists():
            missing_files.append(filename)

    if missing_files:
        print("=" * 70)
        print("ERROR: Missing Required Files")
        print("=" * 70)
        print()
        print("The following files are required but not found:")
        for filename in missing_files:
            print(f"  ✗ {filename}")
        print()
        print("Please ensure all files are in the current directory.")
        print("=" * 70)
        return False

    return True


def main():
    """Main execution pipeline."""
    print()
    print("=" * 70)
    print(" " * 10 + "COBOL-TO-PYTHON MIGRATION EVALUATION")
    print(" " * 15 + "Complete Test & Grading Suite")
    print("=" * 70)
    print()

    # Check dependencies
    print("Step 1: Checking dependencies...")
    if not check_dependencies():
        sys.exit(2)
    print("OK: All required files found")
    print()

    # Run evaluation
    print("Step 2: Running evaluation...")
    print()

    from evaluate_refactor import MigrationEvaluator

    evaluator = MigrationEvaluator(verbose=True)
    results = evaluator.evaluate()

    # Summary
    print()
    print("=" * 70)
    print("FINAL SUMMARY")
    print("=" * 70)
    print()
    print(f"Tests Run:    {results.total_tests}")
    print(f"Passed:       {results.passed_tests}")
    print(f"Failed:       {results.failed_tests}")
    print(f"Errors:       {results.error_tests}")
    print(f"Pass Rate:    {results.pass_rate:.1f}%")
    print(f"Grade:        {results.quality_score}")
    print()

    if results.is_passing:
        print("SUCCESS: MIGRATION PASSED!")
        print()
        print("The Python implementation perfectly preserves all COBOL behaviors.")
        print("All behavioral tests passed.")
        print()
    else:
        print("FAILED: MIGRATION INCOMPLETE")
        print()
        print(f"{results.failed_tests + results.error_tests} test(s) failed.")
        print("Review the detailed report above to identify issues.")
        print()

    print("=" * 70)
    print()

    # Exit with appropriate code
    sys.exit(0 if results.is_passing else 1)


if __name__ == "__main__":
    main()
