import os
import pathlib


def parse_bitmask(s, zero_char='0'):
    ones = 0
    zeroes = 0
    curr = 1
    i = len(s)-1
    while i >= 0:
        if s[i] == zero_char:
            zeroes += curr
        elif s[i] == '1':
            ones += curr
        curr *= 2
        i -= 1
    return ones, zeroes


def parse_input(input_file, zero_char='0'):
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
            code.append(parse_bitmask(line.split()[2].strip(), zero_char))
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


def floating_permutations(e):
    if e == 0:
        return [0]

    p = 1
    while (e & p) == 0:
        p *= 2
    ret = floating_permutations(e-p)
    res = ret.copy()
    for element in ret:
        res.append(element+p)
    return res


def execute_memory_address_decoder(code):
    mem = {}
    bitmask = code[0]
    i = 1
    while i < len(code):
        if isinstance(code[i], tuple): # Bitmask
            bitmask = code[i]
        else:   # Instruction
            addr = (code[i][0] | bitmask[0]) & ~bitmask[1]
            for permutation in floating_permutations(bitmask[1]):
                mem[addr+permutation] = code[i][1]
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

    with open(input_filename, "r") as input_file:
        code = parse_input(input_file, 'X')
    print(execute_memory_address_decoder(code))
