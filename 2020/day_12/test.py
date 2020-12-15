import io
import unittest

from day_12.code import *


class TestParser(unittest.TestCase):

    def setUp(self):
        pass

    def test_parse_code_nominal_case(self):
        s = "F10\n" \
            "N3\n" \
            "F7\n" \
            "R90\n" \
            "F11\n"
        input_file = io.StringIO(s)
        expected_instr = [
            ["F", 10],
            ["N", 3],
            ["F", 7],
            ["R", 90],
            ["F", 11],
        ]

        actual_instr = parse_input(input_file)

        self.assertEqual(expected_instr, actual_instr)


class TestProcessor(unittest.TestCase):

    def setUp(self):
        pass

    def test_travel_nominal_case(self):
        instructions = [
            ["F", 10],
            ["N", 3],
            ["F", 7],
            ["R", 90],
            ["F", 11],
        ]
        boat = Boat()
        travel(boat, instructions)

        self.assertEqual(boat.manhattan_dist, 25)

    def test_travel_by_waypoint_nominal_case(self):
        instructions = [
            ["F", 10],
            ["N", 3],
            ["F", 7],
            ["R", 90],
            ["F", 11],
        ]
        waypoint = Waypoint()
        boat = WaypointBoat(waypoint)
        travel_by_waypoint(boat, waypoint, instructions)

        self.assertEqual(boat.manhattan_dist, 286)



if __name__ == "__main__":
    unittest.main()
