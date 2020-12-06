import pathlib
import os


def parse_forms(input_file):
    """
    Get declaration forms from file
    params:
        input_file - file to parse, opened for reading
    returns:
        list(list(set(str))), list of group's answers,
        a group's answers consists in a list of set of characters
    """
    res = [[]]
    i = 0
    for line in input_file:
        if line == '\n':
            res.append([])
            i += 1
        else:
            res[i].append({c for c in line.strip()})
    return res


def set_union(sets):
    """
    Get union of sets
    params:
        sets (list(set)): list of sets to take union of
    returns:
        set representing union of all sets in `sets`
    """
    res = set()
    for s in sets:
        res = res.union(s)
    return res


if __name__ == "__main__":
    input_filename = os.path.join(pathlib.Path(__file__).parent, "input.txt")
    with open(input_filename, "r") as input_file:
        forms = parse_forms(input_file)
    count = 0
    for form in forms:
        count += len(set_union(form))
    print(count)
