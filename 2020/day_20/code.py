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


def any_matches(tile1, tile2):
    """
    return:
        list((border1, border2))
        where border is one of 'l', 'r', 'b', 't'
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


if __name__ == "__main__":
    input_filename = os.path.join(pathlib.Path(__file__).parent, "input.txt")
    with open(input_filename, "r") as input_file:
        tiles = parse_input(input_file)
    tile_to_matching = {}
    for t_id, t in tiles.items():
        tile_to_matching[t_id] = all_matching(t_id, t, tiles)
    for t_id, s in tile_to_matching.items():
        if len(s) == 2:
            print(t_id)
