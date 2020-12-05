import io
import unittest

from day_02.code import *


class TestProcessor(unittest.TestCase):

    def setUp(self):
        pass

    def test_nominal_case_count_policy(self):
        lines = ["1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc"]
        policy = count_password_policy
        valid_password_count = num_invalid_passwords(policy, lines)

        self.assertEqual(valid_password_count, 2)

    def test_nominal_case_position_policy(self):
        lines = ["1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc"]
        policy = position_password_policy
        valid_password_count = num_invalid_passwords(policy, lines)

        self.assertEqual(valid_password_count, 1)


if __name__ == "__main__":
    unittest.main()
