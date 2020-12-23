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

        self.assertEqual(cups, expected_cups)

    def test_serialize_nominal_case(self):
        cups = [3, 8, 9, 1, 2, 5, 4, 6, 7]
        expected_map = [2, 5, 8, 6, 4, 7, 3, 9, 1]

        serialized = serialize(cups)

        self.assertEqual(serialized, expected_map)

    def test_get_nth_element_nominal_case(self):
        seq_map = [2, 5, 8, 6, 4, 7, 3, 9, 1]

        self.assertEqual(get_nth_element(seq_map, 3, 4), 2)

    def test_shuffle_single_step(self):
        seq_map = [2, 5, 8, 6, 4, 7, 3, 9, 1]
        expected_cups = [3, 2, 8, 9, 1, 5, 4, 6, 7]
        expected_seq_map = serialize(expected_cups)

        self.assertEqual(shuffle(seq_map, 3, 9), 2)

        self.assertEqual(seq_map, expected_seq_map)

    def test_shuffle_ten_times(self):
        seq_map = [2, 5, 8, 6, 4, 7, 3, 9, 1]
        expected_cups = [5, 8, 3, 7, 4, 1, 9, 2, 6]
        expected_seq_map = serialize(expected_cups)

        curr = 3
        for i in range(10):
            curr = shuffle(seq_map, curr, 9)

        self.assertEqual(seq_map, expected_seq_map)


if __name__ == "__main__":
    unittest.main()
