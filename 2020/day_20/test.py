import io
import unittest

from day_20.code import *


class TestParser(unittest.TestCase):

    def setUp(self):
        pass

    def test_parse_input_nominal_case(self):
        s = "Tile 2311:\n" \
            "..\n" \
            "##\n" \
            "\n" \
            "Tile 1951:\n" \
            "#.\n" \
            "#.\n"
        input_file = io.StringIO(s)
        expected_tiles = {
                2311: [
                    ['.', '.'],
                    ['#', '#'],
                ],
                1951: [
                    ['#', '.'],
                    ['#', '.'],
                ],
        }

        tiles = parse_input(input_file)

        self.assertEqual(expected_tiles, tiles)


class TestProcessor(unittest.TestCase):

    def setUp(self):
        pass

    def test_get_borders_nominal_case(self):
        tile = [
            ['.', '.', '.'],
            ['.', '.', '#'],
            ['#', '#', '.'],
        ]
        expected_top = ['.', '.', '.']
        expected_bot = ['#', '#', '.']
        expected_left = ['.', '.', '#']
        expected_right = ['.', '#', '.']

        left, right, bot, top = get_borders(tile)

        self.assertEqual(expected_top, top)
        self.assertEqual(expected_bot, bot)
        self.assertEqual(expected_left, left)
        self.assertEqual(expected_right, right)

    def test_borders_match_no_match(self):
        b1 = ['.', '.', '#']
        b2 = ['.', '#', '#']

        self.assertEqual(borders_match(b1, b2), None)

    def test_borders_match_no_reverse_needed(self):
        b1 = ['.', '.', '#']
        b2 = ['.', '.', '#']

        self.assertEqual(borders_match(b1, b2), False)

    def test_borders_match_reverse_needed(self):
        b1 = ['.', '.', '#']
        b2 = ['#', '.', '.']

        self.assertEqual(borders_match(b1, b2), True)

    def test_any_matches_no_matches(self):
        t1 = [
            ['.', '.', '.'],
            ['.', '.', '#'],
            ['#', '#', '.'],
        ]
        t2 = [
            ['#', '#', '#'],
            ['#', '.', '#'],
            ['#', '#', '#'],
        ]

        self.assertFalse(any_matches(t1, t2))

    def test_any_matches_matches(self):
        t1 = [
            ['.', '.', '.'],
            ['.', '.', '#'],
            ['#', '#', '.'],
        ]
        t2 = [
            ['#', '#', '#'],
            ['#', '.', '#'],
            ['.', '.', '.'],
        ]

        self.assertTrue(any_matches(t1, t2))


if __name__ == "__main__":
    unittest.main()
