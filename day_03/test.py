import io
import unittest

from day_03.code import *


class TestParser(unittest.TestCase):

    def setUp(self):
        pass

    def test_nominal_case(self):
        input_map = "..##.\n" \
                    "#...#\n" \
                    ".#...\n" \
                    "..#.#\n"
        input_file = io.StringIO(input_map)
        expected_treemap = [[False, False, True, True, False],
                            [True, False, False, False, True],
                            [False, True, False, False, False],
                            [False, False, True, False, True]]

        actual_treemap = parse_map(input_file)

        self.assertEqual(expected_treemap, actual_treemap)


class TestProcessor(unittest.TestCase):

    def setUp(self):
        pass

    def test_nominal_case(self):
        treemap = [[False, False, True, True, False],
                   [True, False, False, False, True],
                   [False, True, False, False, False],
                   [False, False, True, False, True]]
        expected_treecount = 2

        actual_treecount = count_trees(treemap, 3, 1)

        self.assertEqual(expected_treecount, actual_treecount)


if __name__ == "__main__":
    unittest.main()
