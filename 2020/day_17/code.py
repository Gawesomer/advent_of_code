import copy
import os
import pathlib


def parse_input(input_file, four_dimensions=False):
    """
    Get dimension from file
    params:
        input_file - file to parse, opened for reading
    returns:
        list(list(list(char)))
    """
    res = {}
    for y, line in enumerate(input_file):
        for x, c in enumerate(line.strip()):
            if four_dimensions:
                res[(x, y, 0, 0)] =  c
            else:
                res[(x, y, 0)] =  c
    return res


def get_value(dimension, coords):
    try:
        return dimension[coords]
    except KeyError:
        return '.'


def set_value(dimension, coords, value):
    dimension[coords] = value


def get_all_actives(dimension):
    """
    return:
        list([x, y, z]) for active positions
    """
    res = []
    for coord, value in dimension.items():
        if value == '#':
            res.append(coord)
    return res


def get_neighbours_indeces(coord):
    """
    return:
        list([x, y, z]) for neighbours
    """
    res = []
    x = coord[0]
    y = coord[1]
    z = coord[2]
    if len(coord) == 4:
        w = coord[3]
    for x_i in [x-1, x, x+1]:
        for y_i in [y-1, y, y+1]:
            for z_i in [z-1, z, z+1]:
                if len(coord) == 4:
                    for w_i in [w-1, w, w+1]:
                        if not (x_i == x and y_i == y and z_i == z and w_i == w):
                            res.append((x_i, y_i, z_i, w_i))
                else:
                        if not (x_i == x and y_i == y and z_i == z):
                            res.append((x_i, y_i, z_i))
    return res


def split_active_inactive(dimension, positions):
    """
    return:
        list(active_positions), list(inactive_positions)
    """
    actives = []
    inactives = []
    for p in positions:
        if get_value(dimension, p) == '#':
            actives.append(p)
        else:
            inactives.append(p)
    return actives, inactives


def get_num_active_neighbours(dimension, coord):
    res = 0
    neighbour_positions = get_neighbours_indeces(coord)
    for p in neighbour_positions:
        if get_value(dimension, p) == '#':
            res += 1
    return res


def to_activate(dimension, actives):
    """
    params:
        actives (list(position))
    return:
        list(positions) for positions to activate
    """
    res = []
    for active in actives:
        neighbour_positions = get_neighbours_indeces(active)
        for p in neighbour_positions:
            if get_value(dimension, p) == '.':
                if get_num_active_neighbours(dimension, p) == 3:
                    res.append(p)
    return res


def to_deactivate(dimension, actives):
    """
    return:
        list(positions) for positions to deactivate
    """
    res = []
    for active in actives:
        num_neighbours = get_num_active_neighbours(dimension, active)
        if num_neighbours not in {2, 3}:
            res.append(active)
    return res


def simulate(dimension):
    """
    return:
        new dimension
    """
    new_dimension = copy.deepcopy(dimension)

    actives = get_all_actives(dimension)
    will_activate = to_activate(dimension, actives)
    will_deactivate = to_deactivate(dimension, actives)

    for p in will_activate:
        set_value(new_dimension, p, '#')

    for p in will_deactivate:
        set_value(new_dimension, p, '.')

    return new_dimension


if __name__ == "__main__":
    input_filename = os.path.join(pathlib.Path(__file__).parent, "input.txt")
    with open(input_filename, "r") as input_file:
        dimension = parse_input(input_file, four_dimensions=True)
    for i in range(6):
        dimension = simulate(dimension)
    print(len(get_all_actives(dimension)))
