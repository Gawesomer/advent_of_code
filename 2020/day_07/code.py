import os
import pathlib


def parse_bagrules(input_file):
    """
    Get bag rules from file
    params:
        input_file - file to parse, opened for reading
    returns:
        dict(color: dict(color: quantity))) where color is a bag color, and
        quantity is the number of bags of said color that can be stored in
        current bag
    """
    res = {}
    for line in input_file:
        curr_bag = line.split("bag")[0].strip()
        res[curr_bag] = {}
        words = line.split()
        i = 4
        while i < len(words):
            if words[i] == "no":
                break
            res[curr_bag]["{} {}".format(words[i+1], words[i+2])] = int(words[i])
            i += 4
    return res


def num_fit(bag_rules, color):
    """
    Get number of bags into which `color` may fit
    params:
        bag_rules (dict): result of `parse_bagrules`
        color (str): color to fit
    returns:
        number of bag colors into which `color` may fit
    """
    fits = set()
    search = [color]
    while search:
        curr = search.pop()
        for rule, bags in bag_rules.items():
            if curr in bags:
                if rule not in fits:
                    search.append(rule)
                    fits.add(rule)
    return len(fits)


def num_bags_in_bag(bag_rules, color):
    """
    Get number of bags in a bag of given `color`
    params:
        bag_rules (dict): result of `parse_bagrules`
        color (str): color of bag
    returns:
        number of bags that fit inside a bag of `color`
    """
    if not bag_rules[color]:
        return 0
    count = 0
    for bag_color, bag_count in bag_rules[color].items():
        count += bag_count + bag_count*num_bags_in_bag(bag_rules, bag_color)
    return count


if __name__ == "__main__":
    input_filename = os.path.join(pathlib.Path(__file__).parent, "input.txt")
    with open(input_filename, "r") as input_file:
        bagrules = parse_bagrules(input_file)
    print(num_fit(bagrules, "shiny gold"))
    print(num_bags_in_bag(bagrules, "shiny gold"))
