import os
import pathlib


def parse_input(input_file):
    """
    Get tiles from file
    params:
        input_file - file to parse, opened for reading
    returns:
        list(list(str))
    """
    res = []
    for line in input_file:
        curr = []
        curr_str = ""
        for c in line.strip():
            if c in {'s', 'n'}:
                curr_str = c
            else:
                if curr_str:
                    curr_str += c
                else:
                    curr_str = c
                curr.append(curr_str)
                curr_str = ""
        res.append(curr.copy())
    return res


def identify_tile(tile, tilecolor_map):
    ne = 0
    e = 0
    for d in tile:
        if d == 'e':
            e += 1
        elif d == 'w':
            e -= 1
        elif d == 'ne':
            ne += 1
        elif d == 'sw':
            ne -= 1
        elif d == 'se':
            e += 1
            ne -= 1
        else:
            e -= 1
            ne += 1
    coord = (e, ne)
    if coord in tilecolor_map:
        tilecolor_map[coord] = not tilecolor_map[coord]
    else:
        tilecolor_map[coord] = True


if __name__ == "__main__":
    input_filename = os.path.join(pathlib.Path(__file__).parent, "input.txt")
    with open(input_filename, "r") as input_file:
        tiles = parse_input(input_file)
    tilecolor_map = {}

    for t in tiles:
        identify_tile(t, tilecolor_map)

    num_black = 0
    for coord, value in tilecolor_map.items():
        if value:
            num_black += 1
    print(num_black)
