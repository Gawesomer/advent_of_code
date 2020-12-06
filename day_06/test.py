import io
import unittest

from day_06.code import *


class TestParser(unittest.TestCase):

    def setUp(self):
        pass

    def test_parse_forms_nominal_case(self):
        s = "abc\n" \
            "\n" \
            "a\n" \
            "b\n" \
            "c\n"
        input_file = io.StringIO(s)
        expected_forms = [
            [
                {'a', 'b', 'c'},
            ],
            [
                {'a'},
                {'b'},
                {'c'},
            ],
        ]

        actual_forms = parse_forms(input_file)

        self.assertEqual(expected_forms, actual_forms)


class TestProcessor(unittest.TestCase):

    def setUp(self):
        pass

    def test_set_union_nominal_case(self):
        sets = [
            {'a', 'b', 'c', 'x'},
            {'a', 'b', 'c', 'y'},
            {'a', 'b', 'c', 'z'},
        ]
        expected_union = {
            'a', 'b', 'c', 'x', 'y', 'z',
        }

        actual_union = set_union(sets)

        self.assertEqual(expected_union, actual_union)


if __name__ == "__main__":
    unittest.main()
