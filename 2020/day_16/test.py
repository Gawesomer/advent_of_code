import io
import unittest

from day_16.code import *


class TestParser(unittest.TestCase):

    def setUp(self):
        pass

    def test_parse_code_nominal_case(self):
        s = "class: 1-3 or 5-7\n" \
            "row: 6-11 or 33-44\n" \
            "seat: 13-40 or 45-50\n" \
            "\n" \
            "your ticket:\n" \
            "7,1,14\n" \
            "\n" \
            "nearby tickets:\n" \
            "7,3,47\n" \
            "40,4,50\n" \
            "55,2,20\n" \
            "38,6,12\n"
        input_file = io.StringIO(s)
        expected_rules = {
            "class": [[1, 3], [5, 7]],
            "row": [[6, 11], [33, 44]],
            "seat": [[13, 40], [45, 50]],
        }
        expected_your_ticket = [7, 1, 14]
        expected_nearby_tickets = [
            [7,3,47],
            [40,4,50],
            [55,2,20],
            [38,6,12],
        ]

        actual_rules, actual_your_ticket, actual_nearby_tickets = parse_input(input_file)

        self.assertEqual(expected_rules, actual_rules)
        self.assertEqual(expected_your_ticket, actual_your_ticket)
        self.assertEqual(expected_nearby_tickets, actual_nearby_tickets)


class TestProcessor(unittest.TestCase):

    def setUp(self):
        pass

    def test_sum_invalid_no_invalid_returns_zero(self):
        ticket = [7,3,47]
        rules = {
            "class": [[1, 3], [5, 7]],
            "row": [[6, 11], [33, 44]],
            "seat": [[13, 40], [45, 50]],
        }

        self.assertEqual(sum_invalid(ticket, rules), 0)

    def test_sum_invalid_invalid_ticket_is_found(self):
        ticket = [40,4,50]
        rules = {
            "class": [[1, 3], [5, 7]],
            "row": [[6, 11], [33, 44]],
            "seat": [[13, 40], [45, 50]],
        }

        self.assertEqual(sum_invalid(ticket, rules), 4)

    def test_scanning_error_rate_nominal_case(self):
        tickets = [
            [7,3,47],
            [40,4,50],
            [55,2,20],
            [38,6,12],
        ]
        rules = {
            "class": [[1, 3], [5, 7]],
            "row": [[6, 11], [33, 44]],
            "seat": [[13, 40], [45, 50]],
        }

        self.assertEqual(scanning_error_rate(tickets, rules), 71)

    def test_remove_invalid_tickets_nominal_case(self):
        tickets = [
            [7,3,47],
            [40,4,50],
            [55,2,20],
            [38,6,12],
        ]
        rules = {
            "class": [[1, 3], [5, 7]],
            "row": [[6, 11], [33, 44]],
            "seat": [[13, 40], [45, 50]],
        }
        valid_tickets = [
            [7,3,47],
        ]

        self.assertEqual(remove_invalid_tickets(tickets, rules), valid_tickets)

    def test_rulename_to_valid_ticket_index_nominal_case(self):
        ticket = [7,3,47]
        rules = {
            "class": [[1, 3], [5, 7]],
            "row": [[6, 11], [33, 44]],
            "seat": [[13, 40], [45, 50]],
        }
        expected_rulename_to_ticket_index = {
            "class": {0,1},
            "row": {0},
            "seat": {2},
        }

        self.assertEqual(rulename_to_valid_ticket_index(ticket, rules), expected_rulename_to_ticket_index)



if __name__ == "__main__":
    unittest.main()
