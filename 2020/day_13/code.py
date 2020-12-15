import os
import pathlib
import sys


def parse_input(input_file):
    """
    Get bus info input from file
    params:
        input_file - file to parse, opened for reading
    returns:
        (ealiest, list(int)) where ealiest is an integer, and the list contains
        None for OOS buses
    """
    res = []
    lines = [line for line in input_file]
    for word in lines[1].split(','):
        if word == 'x':
            res.append(None)
        else:
            res.append(int(word))
    return int(lines[0]), res


def comes_in(bus_id, timestamp):
    """
    Determine in how much time bus with id `bus_id` will arrive
    params:
        bus_id (int)
        timestamp (int) - current time
    returns:
        int
    """
    return bus_id - (timestamp % bus_id)


def find_earliest_bus(buses, timestamp):
    """
    Determine the next earliest bus
    params:
        buses (list(int))
        timestamp (int) - current time
    returns:
        (bus_id, time_until)
    """
    quickest_bus = 0
    shortest_time = sys.maxsize
    curr_time = 0
    for bus in buses:
        if bus:
            curr_time = comes_in(bus, timestamp)
            if curr_time < shortest_time:
                shortest_time = curr_time
                quickest_bus = bus
    return quickest_bus, shortest_time


if __name__ == "__main__":
    input_filename = os.path.join(pathlib.Path(__file__).parent, "input.txt")
    with open(input_filename, "r") as input_file:
        curr_time, buses = parse_input(input_file)
    earliest_bus, wait_time = find_earliest_bus(buses, curr_time)
    print(earliest_bus*wait_time)
    for i, bus in enumerate(buses):
        if bus:
            print("{}, {}".format(bus, i))
    """
    Solution to part 2 was done using my `crt` implementation from Crypto:
    >>> l = [(29, 29), (41-19, 41), (661-29, 661), (13-42, 13), (17-43, 17), (23-52, 23), (521-60, 521), (37-66, 37), (19-79, 19)]
    >>> crt(l)
    >>> (213890632230818, 1463175673841141)
    """
