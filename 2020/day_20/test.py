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

    def test_build_id_map_nominal_case(self):
        s = "Tile 2311:\n" \
            "..##.#..#.\n" \
            "##..#.....\n" \
            "#...##..#.\n" \
            "####.#...#\n" \
            "##.##.###.\n" \
            "##...#.###\n" \
            ".#.#.#..##\n" \
            "..#....#..\n" \
            "###...#.#.\n" \
            "..###..###\n" \
            "\n" \
            "Tile 1951:\n" \
            "#.##...##.\n" \
            "#.####...#\n" \
            ".....#..##\n" \
            "#...######\n" \
            ".##.#....#\n" \
            ".###.#####\n" \
            "###.##.##.\n" \
            ".###....#.\n" \
            "..#.#..#.#\n" \
            "#...##.#..\n" \
            "\n" \
            "Tile 1171:\n" \
            "####...##.\n" \
            "#..##.#..#\n" \
            "##.#..#.#.\n" \
            ".###.####.\n" \
            "..###.####\n" \
            ".##....##.\n" \
            ".#...####.\n" \
            "#.##.####.\n" \
            "####..#...\n" \
            ".....##...\n" \
            "\n" \
            "Tile 1427:\n" \
            "###.##.#..\n" \
            ".#..#.##..\n" \
            ".#.##.#..#\n" \
            "#.#.#.##.#\n" \
            "....#...##\n" \
            "...##..##.\n" \
            "...#.#####\n" \
            ".#.####.#.\n" \
            "..#..###.#\n" \
            "..##.#..#.\n" \
            "\n" \
            "Tile 1489:\n" \
            "##.#.#....\n" \
            "..##...#..\n" \
            ".##..##...\n" \
            "..#...#...\n" \
            "#####...#.\n" \
            "#..#.#.#.#\n" \
            "...#.#.#..\n" \
            "##.#...##.\n" \
            "..##.##.##\n" \
            "###.##.#..\n" \
            "\n" \
            "Tile 2473:\n" \
            "#....####.\n" \
            "#..#.##...\n" \
            "#.##..#...\n" \
            "######.#.#\n" \
            ".#...#.#.#\n" \
            ".#########\n" \
            ".###.#..#.\n" \
            "########.#\n" \
            "##...##.#.\n" \
            "..###.#.#.\n" \
            "\n" \
            "Tile 2971:\n" \
            "..#.#....#\n" \
            "#...###...\n" \
            "#.#.###...\n" \
            "##.##..#..\n" \
            ".#####..##\n" \
            ".#..####.#\n" \
            "#..#.#..#.\n" \
            "..####.###\n" \
            "..#.#.###.\n" \
            "...#.#.#.#\n" \
            "\n" \
            "Tile 2729:\n" \
            "...#.#.#.#\n" \
            "####.#....\n" \
            "..#.#.....\n" \
            "....#..#.#\n" \
            ".##..##.#.\n" \
            ".#.####...\n" \
            "####.#.#..\n" \
            "##.####...\n" \
            "##..#.##..\n" \
            "#.##...##.\n" \
            "\n" \
            "Tile 3079:\n" \
            "#.#.#####.\n" \
            ".#..######\n" \
            "..#.......\n" \
            "######....\n" \
            "####.#..#.\n" \
            ".#...#.##.\n" \
            "#.#####.##\n" \
            "..#.###...\n" \
            "..#.......\n" \
            "..#.###...\n"
        input_file = io.StringIO(s)
        tiles = parse_input(input_file)
        tile_to_matching = get_tile_matching(tiles)
        expected_id_map = [
            [3079, 2473, 1171],
            [2311, 1427, 1489],
            [1951, 2729, 2971],
        ]

        id_map = build_id_map(tile_to_matching, 3)

        self.assertEqual(expected_id_map, id_map)


if __name__ == "__main__":
    unittest.main()
