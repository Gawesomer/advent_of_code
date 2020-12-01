import io
import unittest

from code.challenge_01 import *


class TestParser(unittest.TestCase):

    def setUp(self):
        pass

    def test_nominal_case(self):
        s = "1721\n979\n366\n299\n675\n1456"
        input_file = io.StringIO(s)
        expected_set = set([1721, 979, 366, 299, 675, 1456])

        actual_set = get_numbers_from_file(input_file)

        self.assertEqual(expected_set, actual_set)


class TestProcessor(unittest.TestCase):

    def setUp(self):
        pass

    def test_nominal_case(self):
        nums = set([1721, 979, 366, 299, 675, 1456])
        target = 2020
        expected_pair = (299, 1721)

        actual_pair = get_pair_that_sums_to(nums, target)

        self.assertEqual(expected_pair, actual_pair)


if __name__ == "__main__":
    unittest.main()
