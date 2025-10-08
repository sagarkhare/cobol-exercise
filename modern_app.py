"""
Modern Python Implementation of User Management System

This is a Python migration of the COBOL legacy_app.cob system.
It preserves ALL behavioral characteristics of the original 1987 system.

Original: legacy_app.cob (COBOL, 1987, IBM-370 mainframe)
Migrated: modern_app.py (Python 3, 2025)

Key behaviors preserved:
- Fixed capacity: 100 users, 50 sessions
- 20-character string fields (space-padded/truncated)
- Exact error messages
- Case-sensitive username matching
- Session token generation (6 digits: 100000-999999)
- No session expiration
- Plaintext password storage (historical accuracy)
"""

import random
from typing import Optional, Tuple, List


class UserManagementSystem:
    """
    User Management System - Python implementation of COBOL legacy_app.cob

    This class provides three core operations:
    1. register_user(username, password) -> str
    2. login_user(username, password) -> Tuple[str, Optional[int]]
    3. change_password(token, old_password, new_password) -> str

    All behaviors match the original COBOL implementation exactly.
    """

    # System constraints (from COBOL)
    MAX_USERS = 100
    MAX_SESSIONS = 50
    STRING_LENGTH = 20

    def __init__(self):
        """Initialize empty user and session tables."""
        # User table: list of dicts
        # COBOL equivalent: USER-TABLE with USER-ENTRY OCCURS 100 TIMES
        self.users: List[dict] = []
        self.user_count = 0

        # Session table: list of dicts
        # COBOL equivalent: SESSION-TABLE with SESSION-ENTRY OCCURS 50 TIMES
        self.sessions: List[dict] = []
        self.session_count = 0

    def _normalize_string(self, value: str) -> str:
        """
        Normalize string to COBOL PIC X(20) format.

        COBOL behavior:
        - Strings are exactly 20 characters
        - Shorter strings are right-padded with spaces
        - Longer strings are truncated to 20 characters

        Args:
            value: Input string

        Returns:
            20-character string (padded or truncated)
        """
        if len(value) < self.STRING_LENGTH:
            # Right-pad with spaces
            return value + (" " * (self.STRING_LENGTH - len(value)))
        elif len(value) > self.STRING_LENGTH:
            # Truncate to 20 characters
            return value[:self.STRING_LENGTH]
        else:
            # Exactly 20 characters
            return value

    def _generate_token(self) -> int:
        """
        Generate 6-digit session token.

        COBOL equivalent:
        COMPUTE WS-RANDOM-NUM = FUNCTION RANDOM * 900000 + 100000

        Returns:
            6-digit integer (100000-999999)
        """
        return random.randint(100000, 999999)

    def register_user(self, username: str, password: str) -> str:
        """
        Register a new user.

        COBOL equivalent: REGISTER-USER paragraph (lines 107-136)

        Process:
        1. Normalize username and password to 20 characters
        2. Check if username already exists (duplicate check)
        3. Check if user table is full (capacity check)
        4. Add user to table if all checks pass

        Args:
            username: User's username (will be normalized to 20 chars)
            password: User's password (will be normalized to 20 chars)

        Returns:
            Status message:
            - "SUCCESS: USER REGISTERED!"
            - "ERROR: USERNAME ALREADY EXISTS!"
            - "ERROR: USER DATABASE FULL!"
        """
        # Normalize inputs to COBOL PIC X(20) format
        norm_username = self._normalize_string(username)
        norm_password = self._normalize_string(password)

        # Check if user already exists
        # COBOL: PERFORM VARYING USER-IDX FROM 1 BY 1 UNTIL USER-IDX > USER-COUNT
        for user in self.users[:self.user_count]:
            if user["username"] == norm_username:
                return "ERROR: USERNAME ALREADY EXISTS!"

        # Check if user table is full
        # COBOL: IF USER-COUNT < 100
        if self.user_count >= self.MAX_USERS:
            return "ERROR: USER DATABASE FULL!"

        # Add user to table
        # COBOL: ADD 1 TO USER-COUNT, MOVE ... TO USER-NAME, etc.
        self.user_count += 1
        user_entry = {
            "username": norm_username,
            "password": norm_password,
            "active": 1  # COBOL: USER-ACTIVE PIC 9 VALUE 1
        }

        if len(self.users) < self.user_count:
            self.users.append(user_entry)
        else:
            self.users[self.user_count - 1] = user_entry

        return "SUCCESS: USER REGISTERED!"

    def login_user(self, username: str, password: str) -> Tuple[str, Optional[int]]:
        """
        Login user and create session.

        COBOL equivalent: LOGIN-USER paragraph (lines 138-173)

        Process:
        1. Normalize username and password
        2. Search for user in user table
        3. Verify password matches
        4. Verify user is active
        5. Generate session token
        6. Check if session table has space
        7. Create session entry

        Args:
            username: User's username
            password: User's password

        Returns:
            Tuple of (status_message, token):
            - ("SUCCESS: LOGIN APPROVED", token) - on success
            - ("ERROR: INVALID CREDENTIALS!", None) - on auth failure
            - ("ERROR: SESSION TABLE FULL!", None) - on capacity failure
        """
        # Normalize inputs
        norm_username = self._normalize_string(username)
        norm_password = self._normalize_string(password)

        # Verify credentials
        # COBOL: PERFORM VARYING USER-IDX FROM 1 BY 1 UNTIL USER-IDX > USER-COUNT
        user_found = False
        for user in self.users[:self.user_count]:
            if user["username"] == norm_username:
                if user["password"] == norm_password:
                    if user["active"] == 1:
                        user_found = True
                        break

        if not user_found:
            return ("ERROR: INVALID CREDENTIALS!", None)

        # Generate session token
        # COBOL: PERFORM GENERATE-TOKEN
        token = self._generate_token()

        # Check session table capacity
        # COBOL: IF SESSION-COUNT < 50
        if self.session_count >= self.MAX_SESSIONS:
            return ("ERROR: SESSION TABLE FULL!", None)

        # Create session entry
        # COBOL: ADD 1 TO SESSION-COUNT, MOVE ... TO SESSION-TOKEN, etc.
        self.session_count += 1
        session_entry = {
            "token": token,
            "username": norm_username,
            "active": 1  # COBOL: SESSION-ACTIVE PIC 9 VALUE 1
        }

        if len(self.sessions) < self.session_count:
            self.sessions.append(session_entry)
        else:
            self.sessions[self.session_count - 1] = session_entry

        return ("SUCCESS: LOGIN APPROVED", token)

    def change_password(
        self,
        token: int,
        old_password: str,
        new_password: str
    ) -> str:
        """
        Change user's password using session token.

        COBOL equivalent: CHANGE-PASSWORD paragraph (lines 175-215)

        Process:
        1. Validate session token exists and is active
        2. Retrieve username from session
        3. Find user in user table
        4. Verify old password matches
        5. Update password to new password

        Args:
            token: Session token (6-digit number)
            old_password: Current password
            new_password: New password to set

        Returns:
            Status message:
            - "SUCCESS: PASSWORD CHANGED!"
            - "ERROR: INVALID SESSION TOKEN!"
            - "ERROR: OLD PASSWORD INCORRECT!"
        """
        # Normalize passwords
        norm_old_password = self._normalize_string(old_password)
        norm_new_password = self._normalize_string(new_password)

        # Validate session token
        # COBOL: PERFORM VARYING SESS-IDX FROM 1 BY 1 UNTIL SESS-IDX > SESSION-COUNT
        session_username = None
        for session in self.sessions[:self.session_count]:
            if session["token"] == token and session["active"] == 1:
                session_username = session["username"]
                break

        if session_username is None:
            return "ERROR: INVALID SESSION TOKEN!"

        # Find user and verify old password
        # COBOL: PERFORM VARYING USER-IDX FROM 1 BY 1 UNTIL USER-IDX > USER-COUNT
        for user in self.users[:self.user_count]:
            if user["username"] == session_username:
                if user["password"] == norm_old_password:
                    # Update password
                    # COBOL: MOVE WS-NEW-PASSWORD TO USER-PASSWORD(USER-IDX)
                    user["password"] = norm_new_password
                    return "SUCCESS: PASSWORD CHANGED!"
                else:
                    return "ERROR: OLD PASSWORD INCORRECT!"

        # Should never reach here if session is valid
        return "ERROR: INVALID SESSION TOKEN!"


def main():
    """
    Interactive command-line interface (optional).

    This mimics the COBOL MENU-LOOP behavior for manual testing.
    """
    system = UserManagementSystem()

    print("=" * 70)
    print(" " * 15 + "USER MANAGEMENT SYSTEM v2.0")
    print(" " * 15 + "PYTHON MIGRATION OF COBOL LEGACY APP")
    print("=" * 70)
    print()

    while True:
        print()
        print("MAIN MENU:")
        print("1. REGISTER NEW USER")
        print("2. LOGIN")
        print("3. CHANGE PASSWORD")
        print("4. EXIT")
        choice = input("ENTER CHOICE (1-4): ").strip()

        if choice == "1":
            print("--- USER REGISTRATION ---")
            username = input("ENTER USERNAME: ")
            password = input("ENTER PASSWORD: ")
            result = system.register_user(username, password)
            print(result)

        elif choice == "2":
            print("--- USER LOGIN ---")
            username = input("ENTER USERNAME: ")
            password = input("ENTER PASSWORD: ")
            result, token = system.login_user(username, password)
            print(result)
            if token is not None:
                print(f"YOUR SESSION TOKEN: {token}")

        elif choice == "3":
            print("--- CHANGE PASSWORD ---")
            token_input = input("ENTER SESSION TOKEN: ")
            try:
                token = int(token_input)
            except ValueError:
                print("ERROR: INVALID SESSION TOKEN!")
                continue

            old_password = input("ENTER OLD PASSWORD: ")
            new_password = input("ENTER NEW PASSWORD: ")
            result = system.change_password(token, old_password, new_password)
            print(result)

        elif choice == "4":
            print("SYSTEM SHUTDOWN...")
            break

        else:
            print("INVALID CHOICE. TRY AGAIN.")


if __name__ == "__main__":
    main()
