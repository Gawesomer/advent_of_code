import io
import unittest

from day_15.code import *


class TestProcessor(unittest.TestCase):

    def setUp(self):
        pass

    def test_speak_nums_nominal_case(self):
        nums = [0, 3, 6]

        self.assertEqual(speak_nums(nums, 10), 0)

    def test_speak_nums_larger_case(self):
        nums = [0, 3, 6]

        self.assertEqual(speak_nums(nums, 2020), 436)


if __name__ == "__main__":
    unittest.main()
