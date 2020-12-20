import copy
import os
import pathlib


def parse_input(input_file):
    """
    Get tiles from file
    params:
        input_file - file to parse, opened for reading
    returns:
        dict(id: list(str))
    """
    res = {}
    for line in input_file:
        if line != '\n':
            if line.startswith("Tile"):
                tile_id = line.split(' ')[1].strip()
                tile_id = int(tile_id[:len(tile_id)-1])
            else:
                if tile_id not in res:
                    res[tile_id] = []
                res[tile_id].append([c for c in line.strip()])
    return res


def get_borders(tile):
    """
    return:
        (left, right, bot, top) borders
    """
    bot = tile[len(tile)-1].copy()
    top = tile[0].copy()
    left = [row[0] for row in tile]
    right = [row[len(row)-1] for row in tile]
    
    return left, right, bot, top


def borders_match(border1, border2):
    """
    return:
        None if not match
        False if match no reverse needed
        True if match reverse needed
    """
    b1_rev = border1.copy()
    b1_rev.reverse()

    if border1 == border2:
        return False
    if b1_rev == border2:
        return True
    return None


def which_match(tile1, tile2):
    """
    return:
        reverse_required, (l|r|b|t, l|r|b|t)
    """
    b1 = get_borders(tile1)
    borders1 = {
        'l': b1[0],
        'r': b1[1],
        'b': b1[2],
        't': b1[3],
    }
    b2 = get_borders(tile2)
    borders2 = {
        'l': b2[0],
        'r': b2[1],
        'b': b2[2],
        't': b2[3],
    }
    for side1, border1 in borders1.items():
        for side2, border2 in borders2.items():
            match = borders_match(border1, border2)
            if match == False:
                return False, (side1, side2)
            elif match == True:
                return True, (side1, side2)
    return None, None


def flipv(tile):
    tile_copy = copy.deepcopy(tile)
    i = 0
    while i < len(tile):
        j = 0
        l = len(tile[i])
        while j < l:
            tile[i][j] = tile_copy[i][l-j-1]
            j += 1
        i += 1


def fliph(tile):
    tile_copy = copy.deepcopy(tile)
    i = 0
    l = len(tile)
    while i < len(tile):
        j = 0
        while j < l:
            tile[i][j] = tile_copy[l-i-1][j]
            j += 1
        i += 1


def rotate(tile):
    """
    rotate 90 degrees clockwise
    """
    tile_copy = copy.deepcopy(tile)
    o_x = 0

    c_y = 0
    l = len(tile_copy)

    while o_x < len(tile):
        o_y = 0
        c_x = l-1
        while o_y < len(tile[o_x]):
            tile[o_x][o_y] = tile_copy[c_x][c_y]
            o_y += 1
            c_x -= 1
        o_x += 1
        c_y += 1


def any_matches(tile1, tile2):
    """
    return:
        True if match found
    """
    borders1 = get_borders(tile1)
    borders2 = get_borders(tile2)

    for b1 in borders1:
        for b2 in borders2:
            match = borders_match(b1, b2)
            if match is not None:
                return True
    return False


def all_matching(tile_id, tile, tiles):
    """
    return:
        set(tile_id) of matching tiles
    """
    res = set()
    for t_id, t in tiles.items():
        if t_id != tile_id:
            if any_matches(tile, t):
                res.add(t_id)
    return res


def get_tile_matching(tiles):
    tile_to_matching = {}
    for t_id, t in tiles.items():
        tile_to_matching[t_id] = all_matching(t_id, t, tiles)
    return tile_to_matching


def is_edge(tile_id, tile_to_matching):
    if len(tile_to_matching[tile_id]) <= 3:
        return True
    return False


def get_edge_neighbours(tile_id, tile_to_matching):
    res = set()
    for t in tile_to_matching[tile_id]:
        if is_edge(t, tile_to_matching):
            res.add(t)
    # print("{}: {}".format(tile_id, res))
    return res


def get_corners(tile_to_matching):
    res = set()
    for i, t in tile_to_matching.items():
        if len(t) == 2:
            res.add(i)
    return res


def get_adjacent_to_both(n1, n2, tile_to_matching):
    for i, t in tile_to_matching.items():
        if n1 in t and n2 in t:
            return i
    return None


def build_id_map(tile_to_matching, width):
    """
    return:
        list(list(tile_id))
    """
    res = [[0 for col in range(width)] for row in range(width)]

    i = 0
    while i < width:
        j = 0
        if i == 0:
            curr = next(iter(get_corners(tile_to_matching)))
        else:
            curr = next(iter(get_edge_neighbours(res[i-1][0], tile_to_matching)))
            tile_to_matching[res[i-1][0]].remove(curr)
            tile_to_matching[curr].remove(res[i-1][0])
        res[i][j] = curr
        j += 1
        while j < width:
            if i == 0:
                next_curr = next(iter(get_edge_neighbours(curr, tile_to_matching)))
            else:
                next_curr = get_adjacent_to_both(res[i][j-1], res[i-1][j], tile_to_matching)
            tile_to_matching[curr].remove(next_curr)
            tile_to_matching[next_curr].remove(curr)
            curr = next_curr
            res[i][j] = curr
            j += 1
        i += 1
    return res


def get_corner_matching_borders(tile, n1, n2):
    s = set()
    _, tmp = which_match(tile, n1)
    s.add(tmp[0])
    _, tmp = which_match(tile, n2)
    s.add(tmp[0])
    return s


def position_corner(tile_map):
    _, sides = which_match(tile_map[0][0], tile_map[0][1])
    while sides[0] != 'r':
        rotate(tile_map[0][0])
        _, sides = which_match(tile_map[0][0], tile_map[0][1])
    fliph(tile_map[0][0])


def match_pieces(tile1, tile2, desired_sides):
    for i in range(4):
        rotate(tile2)
        is_reversed, sides = which_match(tile1, tile2)
        if (not is_reversed) and (sides == desired_sides):
            return
    flipv(tile2)
    for i in range(4):
        rotate(tile2)
        is_reversed, sides = which_match(tile1, tile2)
        if (not is_reversed) and (sides == desired_sides):
            return
    flipv(tile2)
    fliph(tile2)
    for i in range(4):
        rotate(tile2)
        is_reversed, sides = which_match(tile1, tile2)
        if (not is_reversed) and (sides == desired_sides):
            return


def build_map(id_map, tiles):
    tile_map = []
    for i, row in enumerate(id_map):
        tile_map.append([])
        for col in row:
            tile_map[i].append(tiles[col])
    i = 0
    while i < len(tile_map):
        j = 0
        if i == 0:
            position_corner(tile_map)
        else:
            match_pieces(tile_map[i-1][j], tile_map[i][j], ('b', 't'))
        while j < len(tile_map[i])-1:
            match_pieces(tile_map[i][j], tile_map[i][j+1], ('r', 'l'))
            j += 1
        i += 1
    return tile_map


def display_tile_map(tile_map, width):
    for row in tile_map:
        i = 0
        while i < width:
            print(i, end=' ')
            for col in row:
                for e in col[i]:
                    print(e, end='')
            print()
            i += 1


def remove_borders(tile_map):
    for row_tile in tile_map:
        for tile in row_tile:
            tile.pop(0)
            tile.pop(len(tile)-1)
            for row in tile:
                row.pop(0)
                row.pop(len(row)-1)


def build_sea_map(tile_map, width):
    sea_map = [[]]
    for row_tile in tile_map:
        i = 0
        while i < width:
            for col in row_tile:
                for e in col[i]:
                    sea_map[len(sea_map)-1].append(e)
            sea_map.append([])
            i += 1
    sea_map.pop(len(sea_map)-1)
    return sea_map


def count_sea_monsters(sea_map):
    """
     #
    ###    ##    ##    #
       #  #  #  #  #  #
    """
    i = 0
    j = 1
    while i < len(sea_map)-3:
        pass


if __name__ == "__main__":
    input_filename = os.path.join(pathlib.Path(__file__).parent, "input.txt")
    with open(input_filename, "r") as input_file:
        tiles = parse_input(input_file)

    tile_to_matching = get_tile_matching(tiles)
    for k, v in tile_to_matching.items():
        print("{}: {}".format(k, v))

    id_map = build_id_map(copy.deepcopy(tile_to_matching), 12)
    for row in id_map:
        print(row)

    tile_map = build_map(id_map, tiles)
    display_tile_map(tile_map, 10)

    print()

    remove_borders(tile_map)
    display_tile_map(tile_map, 8)

    print()

    sea_map = build_sea_map(tile_map, 8)
    for row in sea_map:
        for e in row:
            print(e, end='')
        print()

    total_tags = 0
    for row in sea_map:
        for e in row:
            if e == '#':
                total_tags += 1

    rotate(sea_map)
