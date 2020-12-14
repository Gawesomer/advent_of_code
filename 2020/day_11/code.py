import copy
import os
import pathlib


def parse_seats(input_file):
    """
    Get seat map from file
    params:
        input_file - file to parse, opened for reading
    returns:
        list(list(char)) seat map from file
    """
    res = []
    for line in input_file:
        res.append([c for c in line if c != '\n'])
    return res


def num_adjacent_occupied(seatmap, i, j):
    """
    Finds the number of seats adjacent to seat (i, j) that are occupied
    """
    res = 0
    if i > 0:
        if j > 0:
            if seatmap[i-1][j-1] == '#':
                res += 1
        if seatmap[i-1][j] == '#':
            res += 1
        if j < len(seatmap[i])-1:
            if seatmap[i-1][j+1] == '#':
                res += 1
    if j > 0:
        if seatmap[i][j-1] == '#':
            res += 1
    if j < len(seatmap[i])-1:
        if seatmap[i][j+1] == '#':
            res += 1
    if i < len(seatmap)-1:
        if j > 0:
            if seatmap[i+1][j-1] == '#':
                res += 1
        if seatmap[i+1][j] == '#':
            res += 1
        if j < len(seatmap[i])-1:
            if seatmap[i+1][j+1] == '#':
                res += 1
    return res


def num_visible_occupied(seatmap, i, j):
    """
    Counts number of seats visibly occupied from seat (i, j)
    """
    res = 0
    # Up
    tmp_i = i-1
    tmp_j = j
    while tmp_i >= 0:
        if seatmap[tmp_i][tmp_j] == '#':
            res += 1
            break
        elif seatmap[tmp_i][tmp_j] == 'L':
            break
        tmp_i -= 1
    # Down
    tmp_i = i+1
    tmp_j = j
    while tmp_i < len(seatmap):
        if seatmap[tmp_i][tmp_j] == '#':
            res += 1
            break
        elif seatmap[tmp_i][tmp_j] == 'L':
            break
        tmp_i += 1
    # Left
    tmp_i = i
    tmp_j = j-1
    while tmp_j >= 0:
        if seatmap[tmp_i][tmp_j] == '#':
            res += 1
            break
        elif seatmap[tmp_i][tmp_j] == 'L':
            break
        tmp_j -= 1
    # Right
    tmp_i = i
    tmp_j = j+1
    while tmp_j < len(seatmap[i]):
        if seatmap[tmp_i][tmp_j] == '#':
            res += 1
            break
        elif seatmap[tmp_i][tmp_j] == 'L':
            break
        tmp_j += 1
    # Up-Left
    tmp_i = i-1
    tmp_j = j-1
    while tmp_i >= 0 and tmp_j >= 0:
        if seatmap[tmp_i][tmp_j] == '#':
            res += 1
            break
        elif seatmap[tmp_i][tmp_j] == 'L':
            break
        tmp_i -= 1
        tmp_j -= 1
    # Up-Right
    tmp_i = i-1
    tmp_j = j+1
    while tmp_i >= 0 and tmp_j < len(seatmap[i]):
        if seatmap[tmp_i][tmp_j] == '#':
            res += 1
            break
        elif seatmap[tmp_i][tmp_j] == 'L':
            break
        tmp_i -= 1
        tmp_j += 1
    # Down-Left
    tmp_i = i+1
    tmp_j = j-1
    while tmp_i < len(seatmap) and tmp_j >= 0:
        if seatmap[tmp_i][tmp_j] == '#':
            res += 1
            break
        elif seatmap[tmp_i][tmp_j] == 'L':
            break
        tmp_i += 1
        tmp_j -= 1
    # Down-Right
    tmp_i = i+1
    tmp_j = j+1
    while tmp_i < len(seatmap) and tmp_j < len(seatmap[i]):
        if seatmap[tmp_i][tmp_j] == '#':
            res += 1
            break
        elif seatmap[tmp_i][tmp_j] == 'L':
            break
        tmp_i += 1
        tmp_j += 1
    return res


def step(seatmap, count_adjacent_seats, threshold):
    """
    Simulate one step of seat simulation
    params:
        seatmap (list(list(char)): result of `parse_seats`
        count_adjacent_seats (list(list(char)), int, int) -> int
        threshold (int)
    returns:
        True if change was made, False otherwise
    side-effect:
        `seatmap` has been altered
    """
    seatmap_copy = copy.deepcopy(seatmap)
    changed = False
    i = 0
    while i < len(seatmap):
        j = 0
        while j < len(seatmap[i]):
            if seatmap[i][j] == 'L':
                if count_adjacent_seats(seatmap_copy, i, j) == 0:
                    seatmap[i][j] = '#'
                    changed = True
            elif seatmap[i][j] == '#':
                if count_adjacent_seats(seatmap_copy, i, j) >= threshold:
                    seatmap[i][j] = 'L'
                    changed = True
            j += 1
        i += 1
    return changed


if __name__ == "__main__":
    input_filename = os.path.join(pathlib.Path(__file__).parent, "input.txt")
    with open(input_filename, "r") as input_file:
        seatmap = parse_seats(input_file)
    i = 0
    while step(seatmap, num_visible_occupied, 5):
        print(i)
        i += 1
    res = 0
    for row in seatmap:
        for seat in row:
            if seat == '#':
                res += 1
    print(res)
