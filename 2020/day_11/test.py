import io
import unittest

from day_11.code import *


class TestParser(unittest.TestCase):

    def setUp(self):
        pass

    """
    def test_parse_seats_nominal_case(self):
        s = "L.LL.LL.LL\n" \
            "LLLLLLL.LL\n" \
            "L.L.L..L..\n" \
            "LLLL.LL.LL\n" \
            "L.LL.LL.LL\n" \
            "L.LLLLL.LL\n" \
            "..L.L.....\n" \
            "LLLLLLLLLL\n" \
            "L.LLLLLL.L\n" \
            "L.LLLLL.LL\n"
        input_file = io.StringIO(s)

        actual_nums = parse_numbers(input_file)

        self.assertEqual(expected_nums, actual_nums)
    """


class TestProcessor(unittest.TestCase):

    def setUp(self):
        pass

    def test_step_with_adjacent_change_is_made(self):
        s = "#.#L.L#.##\n" \
            "#LLL#LL.L#\n" \
            "L.L.L..#..\n" \
            "#LLL.##.L#\n" \
            "#.LL.LL.LL\n" \
            "#.LL#L#.##\n" \
            "..L.L.....\n" \
            "#L#LLLL#L#\n" \
            "#.LLLLLL.L\n" \
            "#.#L#L#.##\n"
        input_file = io.StringIO(s)
        seatmap = parse_seats(input_file)

        s = "#.#L.L#.##\n" \
            "#LLL#LL.L#\n" \
            "L.#.L..#..\n" \
            "#L##.##.L#\n" \
            "#.#L.LL.LL\n" \
            "#.#L#L#.##\n" \
            "..L.L.....\n" \
            "#L#L##L#L#\n" \
            "#.LLLLLL.L\n" \
            "#.#L#L#.##\n"
        input_file = io.StringIO(s)
        expected_seatmap = parse_seats(input_file)

        self.assertTrue(step(seatmap, num_adjacent_occupied, 4))
        self.assertEqual(seatmap, expected_seatmap)

    def test_step_with_adjacent_change_is_not_made(self):
        s = "#.#L.L#.##\n" \
            "#LLL#LL.L#\n" \
            "L.#.L..#..\n" \
            "#L##.##.L#\n" \
            "#.#L.LL.LL\n" \
            "#.#L#L#.##\n" \
            "..L.L.....\n" \
            "#L#L##L#L#\n" \
            "#.LLLLLL.L\n" \
            "#.#L#L#.##\n"
        input_file = io.StringIO(s)
        seatmap = parse_seats(input_file)
        expected_seatmap = seatmap.copy()

        self.assertFalse(step(seatmap, num_adjacent_occupied, 4))
        self.assertEqual(seatmap, expected_seatmap)

    def test_num_visible_occupied_all_directions_are_counted(self):
        s = "#..#..#\n" \
            ".......\n" \
            ".......\n" \
            "#..#..#\n" \
            ".......\n" \
            ".......\n" \
            "#..#..#\n"
        input_file = io.StringIO(s)
        seatmap = parse_seats(input_file)

        self.assertEqual(num_visible_occupied(seatmap, 3, 3), 8)

    def test_step_with_visible_change_is_made(self):
        s = "#.L#.L#.L#\n" \
            "#LLLLLL.LL\n" \
            "L.L.L..#..\n" \
            "##L#.#L.L#\n" \
            "L.L#.#L.L#\n" \
            "#.L####.LL\n" \
            "..#.#.....\n" \
            "LLL###LLL#\n" \
            "#.LLLLL#.L\n" \
            "#.L#LL#.L#\n"
        input_file = io.StringIO(s)
        seatmap = parse_seats(input_file)

        s = "#.L#.L#.L#\n" \
            "#LLLLLL.LL\n" \
            "L.L.L..#..\n" \
            "##L#.#L.L#\n" \
            "L.L#.LL.L#\n" \
            "#.LLLL#.LL\n" \
            "..#.L.....\n" \
            "LLL###LLL#\n" \
            "#.LLLLL#.L\n" \
            "#.L#LL#.L#\n"
        input_file = io.StringIO(s)
        expected_seatmap = parse_seats(input_file)

        self.assertTrue(step(seatmap, num_visible_occupied, 5))
        self.assertEqual(seatmap, expected_seatmap)


if __name__ == "__main__":
    unittest.main()
