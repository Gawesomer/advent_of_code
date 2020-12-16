import io
import unittest

from day_09.code import *


class TestParser(unittest.TestCase):

    def setUp(self):
        pass

    def test_parse_code_nominal_case(self):
        s = "35\n" \
            "20\n" \
            "15\n" \
            "25\n" \
            "47\n" \
            "40\n" \
            "62\n" \
            "55\n" \
            "65\n" \
            "95\n" \
            "102\n" \
            "117\n" \
            "150\n" \
            "182\n" \
            "127\n" \
            "219\n" \
            "299\n" \
            "277\n" \
            "309\n" \
            "576\n"
        input_file = io.StringIO(s)
        expected_numbers = [
            35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127,
            219, 299, 277, 309, 576,
        ]

        actual_numbers = parse_numbers(input_file)

        self.assertEqual(expected_numbers, actual_numbers)


class TestProcessor(unittest.TestCase):

    def setUp(self):
        pass

    def test_is_pair_sum_existing_pair_returns_true(self):
        nums = [35, 10, 15, 25, 47]
        target = 40

        self.assertTrue(is_pair_sum(nums, target))

    def test_is_pair_sum_no_pair_returns_false(self):
        nums = [182, 150, 117, 102, 95]
        target = 127

        self.assertFalse(is_pair_sum(nums, target))

    def test_find_first_invalid_nominal_case(self):
        nums = [
            35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127,
            219, 299, 277, 309, 576,
        ]
        preamble = 5

        self.assertEqual(find_first_invalid(nums, preamble), 127)

    def test_find_contiguoous_sum_nominal_case(self):
        nums = [
            35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127,
            219, 299, 277, 309, 576,
        ]
        target = 127

        start, end = find_contiguous_sum(nums, target)

        self.assertEqual(start, 2)
        self.assertEqual(end, 5)



if __name__ == "__main__":
    unittest.main()
