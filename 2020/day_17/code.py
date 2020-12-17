import copy
import os
import pathlib


def parse_input(input_file):
    """
    Get dimension from file
    params:
        input_file - file to parse, opened for reading
    returns:
        list(list(list(char)))
    """
    res = {}
    for i, line in enumerate(input_file):
        res[i] = {j: c for j, c in enumerate(line) if c != '\n'}
    return {0: res}


def get_value(dimension, x, y, z):
    try:
        return dimension[z][y][x]
    except KeyError:
        return '.'


def set_value(dimension, x, y, z, value):
    if z not in dimension:
        dimension[z] = {}
    if y not in dimension[z]:
        dimension[z][y] = {}
    dimension[z][y][x] = value


def get_all_actives(dimension):
    """
    return:
        list([x, y, z]) for active positions
    """
    res = []
    for z_index, z_pane in dimension.items():
        for y_index, y_pane in z_pane.items():
            for x_index, value in y_pane.items():
                if value == '#':
                    res.append([x_index, y_index, z_index])
    return res


def get_neighbours_indeces(x, y, z):
    """
    return:
        list([x, y, z]) for neighbours
    """
    res = []
    for x_i in [x-1, x, x+1]:
        for y_i in [y-1, y, y+1]:
            for z_i in [z-1, z, z+1]:
                if not (x_i == x and y_i == y and z_i == z):
                    res.append([x_i, y_i, z_i])
    return res


def split_active_inactive(dimension, positions):
    """
    return:
        list(active_positions), list(inactive_positions)
    """
    actives = []
    inactives = []
    for p in positions:
        if get_value(dimension, p[0], p[1], p[2]) == '#':
            actives.append(p)
        else:
            inactives.append(p)
    return actives, inactives


def get_num_active_neighbours(dimension, x, y, z):
    res = 0
    neighbour_positions = get_neighbours_indeces(x, y, z)
    for p in neighbour_positions:
        if get_value(dimension, p[0], p[1], p[2]) == '#':
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
        neighbour_positions = get_neighbours_indeces(active[0], active[1], active[2])
        for p in neighbour_positions:
            if get_value(dimension, p[0], p[1], p[2]) == '.':
                if get_num_active_neighbours(dimension, p[0], p[1], p[2]) == 3:
                    res.append(p)
    return res


def to_deactivate(dimension, actives):
    """
    return:
        list(positions) for positions to deactivate
    """
    res = []
    for active in actives:
        num_neighbours = get_num_active_neighbours(dimension, active[0], active[1], active[2])
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
        set_value(new_dimension, p[0], p[1], p[2], '#')

    for p in will_deactivate:
        set_value(new_dimension, p[0], p[1], p[2], '.')

    return new_dimension


if __name__ == "__main__":
    input_filename = os.path.join(pathlib.Path(__file__).parent, "input.txt")
    with open(input_filename, "r") as input_file:
        dimension = parse_input(input_file)
    for i in range(6):
        dimension = simulate(dimension)
    print(len(get_all_actives(dimension)))
