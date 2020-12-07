import io
import unittest

from day_07.code import *


class TestParser(unittest.TestCase):

    def setUp(self):
        pass

    def test_parse_bagrules_nominal_case(self):
        s = "light red bags contain 2 muted yellow bags.\n" \
            "dark orange bags contain 3 bright white bags, 4 muted yellow bags.\n" \
            "bright white bags contain 1 shiny gold bag.\n"
        input_file = io.StringIO(s)
        expected_bagrules = {
            "light red": {
                "muted yellow": 2,
            },
            "dark orange": {
                "bright white": 3,
                "muted yellow": 4,
            },
            "bright white": {
                "shiny gold": 1,
            },
        }

        actual_bagrules = parse_bagrules(input_file)

        self.assertEqual(expected_bagrules, actual_bagrules)

    def test_parse_bagrules_rule_contains_no_other_bags(self):
        s = "faded gold bags contain no other bags.\n"
        input_file = io.StringIO(s)
        expected_bagrules = {
            "faded gold": {},
        }

        actual_bagrules = parse_bagrules(input_file)

        self.assertEqual(expected_bagrules, actual_bagrules)


class TestProcessor(unittest.TestCase):

    def setUp(self):
        pass

    def test_num_fit_nominal_case(self):
        bagrules = {
            "light red": {
                "muted yellow": 2,
            },
            "dark orange": {
                "bright white": 3,
                "muted yellow": 4,
            },
            "bright white": {
                "shiny gold": 1,
            },
        }
        color = "shiny gold"

        self.assertEqual(num_fit(bagrules, color), 2)


if __name__ == "__main__":
    unittest.main()
