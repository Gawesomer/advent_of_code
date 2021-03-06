import os
import pathlib


def parse_code(input_file):
    """
    Get boot code from file
    params:
        input_file - file to parse, opened for reading
    returns:
        list((instruction: offset)) where instruction is one of 
        {"nop", "jmp", "acc"} and offset is an integer
    """
    res = []
    for line in input_file:
        words = line.split()
        res.append([words[0], int(words[1])])
    return res


def run_code(code):
    """
    Runs code until repeated instruction
    params:
        code (list): result of `parse_code`
    returns:
        accumulator value before repeat
    """
    already_run = set()
    accumulator = 0
    i = 0
    while (i not in already_run) and (i < len(code)):
        already_run.add(i)
        if code[i][0] == "nop":
            i += 1
        elif code[i][0] == "acc":
            accumulator += code[i][1]
            i += 1
        elif code[i][0] == "jmp":
            i += code[i][1]
    return ((i == len(code)), accumulator)



if __name__ == "__main__":
    input_filename = os.path.join(pathlib.Path(__file__).parent, "input.txt")
    with open(input_filename, "r") as input_file:
        code = parse_code(input_file)
    print(run_code(code)[1])
    jmp_or_nop = set()
    for i, instruction in enumerate(code):
        if instruction[0] == "jmp" or instruction[0] == "nop":
            jmp_or_nop.add(i)

    for i in jmp_or_nop:
        if code[i][0] == "jmp":
            code[i][0] = "nop"
            res = run_code(code)
            if res[0]:
                print(res[1])
                break
            code[i][0] = "jmp"
        else:
            code[i][0] = "jmp"
            res = run_code(code)
            if res[0]:
                print(res[1])
                break
            code[i][0] = "nop"
