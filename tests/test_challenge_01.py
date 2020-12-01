import io
import unittest

from code.challenge_01 import *


class TestParser(unittest.TestCase):

    def setUp(self):
        pass

    def test_nominal_case(self):
        s = "1721\n979\n366\n299\n675\n1456"
        input_file = io.StringIO(s)
        expected_collection = [1721, 979, 366, 299, 675, 1456]

        actual_collection = get_numbers_from_file(input_file)

        self.assertEqual(expected_collection, actual_collection)


class TestProcessor(unittest.TestCase):

    def setUp(self):
        pass

    def test_nominal_case(self):
        nums = [1721, 979, 366, 299, 675, 1456]
        target = 2020
        expected_pair = (299, 1721)

        actual_pair = get_x_numbers_that_sum_to_target(nums, 2, target)

        self.assertEqual(expected_pair, actual_pair)

    def test_triple_sum(self):
        nums = [1721, 979, 366, 299, 675, 1456]
        target = 2020
        expected_list = (366, 675, 979)

        actual_pair = get_x_numbers_that_sum_to_target(nums, 3, target)

        self.assertEqual(expected_list, actual_pair)


if __name__ == "__main__":
    unittest.main()
