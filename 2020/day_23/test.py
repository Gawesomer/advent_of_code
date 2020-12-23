import io
import unittest

from day_23.code import *


class TestParser(unittest.TestCase):

    def setUp(self):
        pass


class TestProcessor(unittest.TestCase):

    def setUp(self):
        pass

    def test_step_single_step(self):
        cups = [3, 8, 9, 1, 2, 5, 4, 6, 7]
        expected_cups = [3, 2, 8, 9, 1, 5, 4, 6, 7]

        step(cups, 3)

        self.assertEqual(cups, expected_cups)

    def test_step_ten_times(self):
        cups = [3, 8, 9, 1, 2, 5, 4, 6, 7]
        expected_cups = [5, 8, 3, 7, 4, 1, 9, 2, 6]

        curr = 3
        for i in range(10):
            curr = step(cups, curr)
            print("{} {}".format(curr, cups))

        self.assertEqual(cups, expected_cups)


if __name__ == "__main__":
    unittest.main()
