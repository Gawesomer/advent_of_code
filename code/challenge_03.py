def parse_map(input_file):
    """
    Parse tree map from input file
    params:
        input_file - file to parse, opened for reading
    returns:
        two dimensional array of booleans
        True indicates a tree, False otherwise
    """
    res = []
    for i, line in enumerate(input_file):
        res.append([])
        for c in line:
            if c == '#':
                res[i].append(True)
            elif c == '.':
                res[i].append(False)

    return res


def count_trees(treemap, x_eps, y_eps):
    """
    Counts the number of trees from top left corner down the diagonal with
    slope x_eps/y_eps
    params:
        treemap - two-dimensional boolean array representing tree locations
        x_eps - horizontal displacement
        y_eps - vertical displacement
    returns:
        number of tree on slope
    """
    x = 0
    y = 0
    treecount = 0
    while y < len(treemap):
        if treemap[y][x]:
            treecount += 1
        x = (x+x_eps)%len(treemap[y])
        y += y_eps
    return treecount



if __name__ == "__main__":
    with open("input_03.txt", "r") as input_file:
        treemap = parse_map(input_file)
    print(count_trees(treemap, 3, 1))
