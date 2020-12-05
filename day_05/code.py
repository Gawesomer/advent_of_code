import pathlib
import os


def parse_boardingpass(input_file):
    """
    Get boarding passes from file
    params:
        input_file - file to parse, opened for reading
    returns:
        collection of (row, col) tuples
    """
    res = []
    for line in input_file:
        res.append((line[:7], line.strip()[-3:]))
    return res


def btoi(binary, bitset):
    """
    Translate binary string to integer
    params:
        binary - binary string
        bitset - character that should be interpreted as a set bit
                 any other character will be interpreted as an unset bit
    returns:
        integer value of `binary`
    """
    value = 0
    exp = 1
    for i in range(-1, (-1*len(binary))-1, -1):
        if binary[i] == bitset:
            value += exp
        exp *= 2
    return value


if __name__ == "__main__":
    input_filename = os.path.join(pathlib.Path(__file__).parent, "input.txt")
    with open(input_filename, "r") as input_file:
        boardingpasses = parse_boardingpass(input_file)
    all_bpass = set()
    max_bpass = 0
    for bpass in boardingpasses:
        curr_bpass = (btoi(bpass[0], 'B')*8)+btoi(bpass[1], 'R')
        all_bpass.add(curr_bpass)
        if curr_bpass > max_bpass:
            max_bpass = curr_bpass
    print(max_bpass)
    for i in range(0, 1024):
        if i not in all_bpass and ((i-1 in all_bpass) and (i+1 in all_bpass)):
            print("missing: {}".format(i))
