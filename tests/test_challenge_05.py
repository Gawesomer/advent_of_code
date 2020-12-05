import io
import unittest

from code.challenge_05 import *


class TestParser(unittest.TestCase):

    def setUp(self):
        pass

    def test_parse_boardingpass_nominal_case(self):
        s = "FBBBBBBRRL\n" \
            "BBFFFBBLRL\n"
        input_file = io.StringIO(s)
        expected_boardingpasses = [
            ("FBBBBBB", "RRL"),
            ("BBFFFBB", "LRL"),
        ]

        actual_boardingpasses = parse_boardingpass(input_file)

        self.assertEqual(expected_boardingpasses, actual_boardingpasses)

    def test_btoi_nominal_row(self):
        binary = "FBFBBFF"
        expected_value = 44

        actual_value = btoi(binary, 'B')

        self.assertEqual(expected_value, actual_value)

    def test_btoi_nominal_col(self):
        binary = "RLR"
        expected_value = 5

        actual_value = btoi(binary, 'R')

        self.assertEqual(expected_value, actual_value)


class TestProcessor(unittest.TestCase):

    def setUp(self):
        pass


if __name__ == "__main__":
    unittest.main()
