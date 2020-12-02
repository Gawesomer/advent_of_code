import io
import unittest

from code.challenge_02 import *


class TestProcessor(unittest.TestCase):

    def setUp(self):
        pass

    def test_nominal_case(self):
        lines = ["1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc"]

        self.assertEqual(num_invalid_passwords(lines), 2)


if __name__ == "__main__":
    unittest.main()
