import io
import unittest

from day_10.code import *


class TestParser(unittest.TestCase):

    def setUp(self):
        pass

    def test_parse_code_nominal_case(self):
        s = "16\n" \
            "10\n" \
            "15\n" \
            "5\n" \
            "1\n" \
            "11\n" \
            "7\n" \
            "19\n" \
            "6\n" \
            "12\n" \
            "4\n"
        input_file = io.StringIO(s)
        expected_nums = [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4]

        actual_nums = parse_numbers(input_file)

        self.assertEqual(expected_nums, actual_nums)


class TestProcessor(unittest.TestCase):

    def setUp(self):
        pass

    def test_find_joltage_differences_nominal_case(self):
        nums = [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4]
        expected_diffs = [7, 0, 5]

        self.assertEqual(find_joltage_differences(nums), expected_diffs)

    def test_find_joltage_arrangements_nominal_case(self):
        nums = [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4, 0, 22]

        self.assertEqual(find_joltage_arrangements(sorted(nums)), 8)


if __name__ == "__main__":
    unittest.main()
