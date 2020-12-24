import copy
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


def neighbour_coordinates(coord):
    return [
        (coord[0]+1, coord[1]),
        (coord[0], coord[1]+1),
        (coord[0]-1, coord[1]),
        (coord[0], coord[1]-1),
        (coord[0]-1, coord[1]+1),
        (coord[0]+1, coord[1]-1),
    ]


def num_black_adjacent(coord, tilecolor_map):
    res = 0
    for c in neighbour_coordinates(coord):
        if c in tilecolor_map:
            if tilecolor_map[c]:
                res += 1
    return res


def get_white_adjacent(coord, tilecolor_map):
    res = []
    for c in neighbour_coordinates(coord):
        if not tilecolor_map.get(c, False):
            res.append(c)
    return res


def get_black_tiles(tilecolor_map):
    res = []
    for coord, value in tilecolor_map.items():
        if value:
            res.append(coord)
    return res


def step(tilecolor_map):
    tilecolor_map_copy = copy.deepcopy(tilecolor_map)

    black_tiles = get_black_tiles(tilecolor_map)
    for b in black_tiles:
        white_adj = get_white_adjacent(b, tilecolor_map)
        black_adj = num_black_adjacent(b, tilecolor_map)
        if black_adj == 0 or black_adj > 2:
            tilecolor_map_copy[b] = False
        for w in white_adj:
            if num_black_adjacent(w, tilecolor_map) == 2:
                tilecolor_map_copy[w] = True

    return tilecolor_map_copy


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

    for i in range(100):
        tilecolor_map = step(tilecolor_map)
    print(len(get_black_tiles(tilecolor_map)))
