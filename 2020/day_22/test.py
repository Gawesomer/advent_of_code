import io
import unittest

from day_22.code import *


class TestParser(unittest.TestCase):

    def setUp(self):
        pass

    def test_parse_input_nominal_case(self):
        s = "Player 1:\n" \
            "9\n" \
            "2\n" \
            "6\n" \
            "3\n" \
            "1\n" \
            "\n" \
            "Player 2:\n" \
            "5\n" \
            "8\n" \
            "4\n" \
            "7\n" \
            "10\n"
        input_file = io.StringIO(s)
        expected_stck1 = [9, 2, 6, 3, 1]
        expected_stck2 = [5, 8, 4, 7, 10]

        stck1, stck2 = parse_input(input_file)

        self.assertEqual(expected_stck1, stck1)
        self.assertEqual(expected_stck2, stck2)


class TestProcessor(unittest.TestCase):

    def setUp(self):
        pass

    def test_recursive_combate_nominal_case(self):
        stck1 = [9, 2, 6, 3, 1]
        stck2 = [5, 8, 4, 7, 10]

        recursive_combat(stck1, stck2)

        self.assertEqual(count_score(stck2), 291)


if __name__ == "__main__":
    unittest.main()
