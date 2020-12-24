import io
import unittest

from day_24.code import *


class TestParser(unittest.TestCase):

    def setUp(self):
        pass

    def test_parse_input_nominal_case(self):
        s = "sesenwnenenewseeswwswswwnenewsewsw\n" \
            "nwwswee\n"
        input_file = io.StringIO(s)
        expected_tiles = [
            ["se", "se", "nw", "ne", "ne", "ne", "w", "se", "e", "sw", "w", "sw", "sw", "w", "ne", "ne", "w", "se", "w", "sw"],
            ["nw", "w", "sw", "e", "e"],
        ]

        tiles = parse_input(input_file)

        self.assertEqual(expected_tiles, tiles)


class TestProcessor(unittest.TestCase):

    def setUp(self):
        pass

    def test_identify_tile_single_flip(self):
        tiles = [
            ["e", "se", "w"],
        ]
        tilecolor_map = {}
        expected_tilecolor_map = {
            (1, -1): True,
        }

        identify_tile(tiles[0], tilecolor_map)

        self.assertEqual(tilecolor_map, expected_tilecolor_map)

    def test_identify_tile_reference_tile(self):
        tiles = [
            ["nw", "w", "sw", "e", "e"],
        ]
        tilecolor_map = {}
        expected_tilecolor_map = {
            (0, 0): True,
        }

        identify_tile(tiles[0], tilecolor_map)

        self.assertEqual(tilecolor_map, expected_tilecolor_map)


    def test_identify_tile_nominal_case(self):
        s = "sesenwnenenewseeswwswswwnenewsewsw\n" \
            "neeenesenwnwwswnenewnwwsewnenwseswesw\n" \
            "seswneswswsenwwnwse\n" \
            "nwnwneseeswswnenewneswwnewseswneseene\n" \
            "swweswneswnenwsewnwneneseenw\n" \
            "eesenwseswswnenwswnwnwsewwnwsene\n" \
            "sewnenenenesenwsewnenwwwse\n" \
            "wenwwweseeeweswwwnwwe\n" \
            "wsweesenenewnwwnwsenewsenwwsesesenwne\n" \
            "neeswseenwwswnwswswnw\n" \
            "nenwswwsewswnenenewsenwsenwnesesenew\n" \
            "enewnwewneswsewnwswenweswnenwsenwsw\n" \
            "sweneswneswneneenwnewenewwneswswnese\n" \
            "swwesenesewenwneswnwwneseswwne\n" \
            "enesenwswwswneneswsenwnewswseenwsese\n" \
            "wnwnesenesenenwwnenwsewesewsesesew\n" \
            "nenewswnwewswnenesenwnesewesw\n" \
            "eneswnwswnwsenenwnwnwwseeswneewsenese\n" \
            "neswnwewnwnwseenwseesewsenwsweewe\n" \
            "wseweeenwnesenwwwswnew\n"
        input_file = io.StringIO(s)
        tiles = parse_input(input_file)
        tilecolor_map = {}

        for t in tiles:
            identify_tile(t, tilecolor_map)

        num_black = 0
        for coord, value in tilecolor_map.items():
            if value:
                num_black += 1

        self.assertEqual(num_black, 10)


if __name__ == "__main__":
    unittest.main()
