import os
import pathlib


def parse_input(input_file):
    """
    Get navigation instructions from file
    params:
        input_file - file to parse, opened for reading
    returns:
        list((instruction, offset)) where instruction is one of 
        {"N", "S", "E", "W", "L", "R", "F"} and offset is an integer
    """
    res = []
    for line in input_file:
        res.append([line[0], int(line[1:])])
    return res


class Boat():

    def __init__(self, start_east=0, start_north=0, start_direction='E'):
        self.east = start_east
        self.north = start_north
        self.direction = start_direction

    @property
    def manhattan_dist(self):
        return (abs(self.east) + abs(self.north))

    def move(self, direction, offset):
        """
        Move in `direction` by given `offset`
        params:
            direction (str): one of {"N", "S", "E", "W"}
            offset (int)
        side-effect:
            moves the boat
        """
        if direction == 'N':
            self.north += offset
        elif direction == 'S':
            self.north -= offset
        elif direction == 'E':
            self.east += offset
        elif direction == 'W':
            self.east -= offset

    def _turn_right_once(self):
        if self.direction == 'E':
            self.direction = 'S'
        elif self.direction == 'S':
            self.direction = 'W'
        elif self.direction == 'W':
            self.direction = 'N'
        elif self.direction == 'N':
            self.direction = 'E'

    def _turn_left_once(self):
        if self.direction == 'E':
            self.direction = 'N'
        elif self.direction == 'N':
            self.direction = 'W'
        elif self.direction == 'W':
            self.direction = 'S'
        elif self.direction == 'S':
            self.direction = 'E'

    def turn(self, direction, angle):
        """
        Turn in `direction` by `angle` degrees
        params:
            direction (str): one of {"R", "L"}
            angle (int): mutliple of 90
        side-effect:
            turns boat
        """
        if direction == 'R':
            while angle > 0:
                self._turn_right_once()
                angle -= 90
        elif direction == 'L':
            while angle > 0:
                self._turn_left_once()
                angle -= 90

    def forward(self, offset):
        """
        Move in current direction by `offset`
        """
        self.move(self.direction, offset)


class Waypoint(Boat):

    def __init__(self, start_east=10, start_north=1, start_direction='E'):
        super(Waypoint, self).__init__(start_east=start_east, start_north=start_north)

    def _turn_right_once(self):
        tmp = self.east
        self.east = self.north
        self.north = -1*tmp

    def _turn_left_once(self):
        tmp = self.east
        self.east = -1*self.north
        self.north = tmp


class WaypointBoat(Boat):

    def __init__(self, waypoint):
        super(WaypointBoat, self).__init__()
        self.waypoint = waypoint

    def forward(self, offset):
        for i in range(offset):
            self.east += self.waypoint.east
            self.north += self.waypoint.north


def travel(boat, instructions):
    """
    Moves boat by following instructions
    """
    for instruction in instructions:
        if instruction[0] in {'N', 'S', 'E', 'W'}:
            boat.move(instruction[0], instruction[1])
        elif instruction[0] in {'R', 'L'}:
            boat.turn(instruction[0], instruction[1])
        elif instruction[0] == 'F':
            boat.forward(instruction[1])


def travel_by_waypoint(boat, waypoint, instructions):
    for instruction in instructions:
        if instruction[0] in {'N', 'S', 'E', 'W'}:
            waypoint.move(instruction[0], instruction[1])
        elif instruction[0] in {'R', 'L'}:
            waypoint.turn(instruction[0], instruction[1])
        elif instruction[0] == 'F':
            boat.forward(instruction[1])
        # print("boat - east {}, north {}".format(boat.east, boat.north))
        # print("waypoint - east {}, north {}".format(waypoint.east, waypoint.north))


if __name__ == "__main__":
    input_filename = os.path.join(pathlib.Path(__file__).parent, "input.txt")
    with open(input_filename, "r") as input_file:
        instructions = parse_input(input_file)
    boat = Boat()
    travel(boat, instructions)
    print(boat.manhattan_dist)

    waypoint = Waypoint()
    boat = WaypointBoat(waypoint)
    travel_by_waypoint(boat, waypoint, instructions)
    print(boat.manhattan_dist)
