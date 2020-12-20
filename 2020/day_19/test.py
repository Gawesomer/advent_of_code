import io
import unittest

from day_19.code import *


class TestParser(unittest.TestCase):

    def setUp(self):
        pass

    def test_parse_input_nominal_case(self):
        s = "0: 4 1 5\n" \
            "1: 2 3 | 3 2\n" \
            "2: 4 4 | 5 5\n" \
            "3: 4 5 | 5 4\n" \
            '4: "a"\n' \
            '5: "b"\n' \
            "\n" \
            "ababbb\n" \
            "bababa\n" \
            "abbbab\n" \
            "aaabbb\n" \
            "aaaabbb\n"
        input_file = io.StringIO(s)
        expected_rules = {
                0: [[4, 1, 5]],
                1: [[2, 3], [3, 2]],
                2: [[4, 4], [5, 5]],
                3: [[4, 5], [5, 4]],
                4: 'a',
                5: 'b',
        }
        expected_messages = [
            "ababbb",
            "bababa",
            "abbbab",
            "aaabbb",
            "aaaabbb",
        ]

        rules, messages = parse_input(input_file)

        self.assertEqual(expected_rules, rules)
        self.assertEqual(expected_messages, messages)


class TestProcessor(unittest.TestCase):

    def setUp(self):
        pass

    def get_all_sub_indeces_nominal_case(self):
        rule = [[2, 3], [3, 2, 4]]
        expected = set([2, 3, 4])
        self.assertEqual(get_all_sub_indeces(rule), expected)

    def test_matches_rule_nominal_case(self):
        rules = {
                0: [[4, 1, 5]],
                1: [[2, 3], [3, 2]],
                2: [[4, 4], [5, 5]],
                3: [[4, 5], [5, 4]],
                4: 'a',
                5: 'b',
        }
        processed = process_rules(rules)
        self.assertTrue('abbbab' in processed[0])


if __name__ == "__main__":
    unittest.main()
