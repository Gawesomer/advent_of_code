import os
import pathlib


def parse_bitmask(s):
    ones = 0
    zeroes = 0
    curr = 1
    i = len(s)-1
    while i >= 0:
        if s[i] == '0':
            zeroes += curr
        elif s[i] == '1':
            ones += curr
        curr *= 2
        i -= 1
    return ones, zeroes


def parse_input(input_file):
    """
    Get instructions from file
    params:
        input_file - file to parse, opened for reading
    returns:
        list of instructions
        bitmask are stored in tuples
        instructions are stored in lists
    """
    code = []
    for line in input_file:
        if line[0:3] == "mem":
            curr_split = line.split()
            code.append([int(curr_split[0][4:-1]), int(curr_split[2].strip())])
        else:
            code.append(parse_bitmask(line.split()[2].strip()))
    return code


# value | bitmask[0]
# value ^ bitmask[1]
def execute(code):
    """
    Executes the code and returns sum of values in memory upon completion
    """
    mem = {}
    bitmask = code[0]
    i = 1
    while i < len(code):
        if isinstance(code[i], tuple): # Bitmask
            bitmask = code[i]
        else:   # Instruction
            mem[code[i][0]] = (code[i][1] | bitmask[0]) & ~bitmask[1]
        i += 1
    res = 0
    for value in mem.values():
        res += value
    return res


if __name__ == "__main__":
    input_filename = os.path.join(pathlib.Path(__file__).parent, "input.txt")
    with open(input_filename, "r") as input_file:
        code = parse_input(input_file)
    print(execute(code))
