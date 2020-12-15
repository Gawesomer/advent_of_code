import io
import unittest

from day_13.code import *


class TestParser(unittest.TestCase):

    def setUp(self):
        pass

    def test_parse_code_nominal_case(self):
        s = "939\n" \
            "7,13,x,x,59,x,31,19\n"
        input_file = io.StringIO(s)
        expected_earliest = 939
        expected_buses = [
            7, 13, None, None, 59, None, 31, 19,
        ]

        actual_earliest, actual_buses = parse_input(input_file)

        self.assertEqual(expected_earliest, actual_earliest)
        self.assertEqual(expected_buses, actual_buses)


class TestProcessor(unittest.TestCase):

    def setUp(self):
        pass

    def test_find_earliest_nominal_case(self):
        time = 939
        buses = [
            7, 13, None, None, 59, None, 31, 19,
        ]

        earliest_bus, wait_time = find_earliest_bus(buses, time)

        self.assertEqual(earliest_bus, 59)
        self.assertEqual(wait_time, 5)



if __name__ == "__main__":
    unittest.main()
