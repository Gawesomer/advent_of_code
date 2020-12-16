import os
import pathlib


def parse_input(input_file):
    """
    Get ticket notes from file
    params:
        input_file - file to parse, opened for reading
    returns:
        dict(str: list([min, max]))
        your_ticket = list(int)
        nearby_tickets = list(list(int))
    """
    rules = {}
    nearby_tickets = []
    state = 0   # {0: rules, 1: your_ticket, 2: nearby_tickets}
    for line in input_file:
        if line == '\n':
            state += 1
        else:
            if state == 0:
                col_index = line.find(':')
                ranges = [r.strip() for r in line[col_index+2:].split(' or ')]
                rules[line[:col_index]] = [[int(r.split('-')[0]), int(r.split('-')[1])] for r in ranges]
            elif state == 1:
                if line != "your ticket:\n":
                    your_ticket = [int(n) for n in line.split(',')]
            else:
                if line != "nearby tickets:\n":
                    nearby_tickets.append([int(n) for n in line.split(',')])
    return rules, your_ticket, nearby_tickets


def sum_invalid(ticket, rules):
    """
    Sum invalid values in single ticket
    """
    res = 0
    for value in ticket:
        found = False
        for rule in rules.values():
            if (value >= rule[0][0] and value <= rule[0][1]) \
                    or (value >= rule[1][0] and value <= rule[1][1]):
                found = True
                break
        if not found:
            res += value
    return res


def scanning_error_rate(tickets, rules):
    """
    Sum invalid values over all tickets
    """
    res = 0
    for ticket in tickets:
        res += sum_invalid(ticket, rules)
    return res


def remove_invalid_tickets(tickets, rules):
    """
    Return new list of tickets with invalid tickets removed
    """
    res = [t for t in tickets if sum_invalid(t, rules) == 0]
    return res


def rulename_to_valid_ticket_index(ticket, rules):
    """
    return:
        dict(rule_name: list(int))
    """
    res = {}
    for i, value in enumerate(ticket):
        for rule_name, rule_range in rules.items():
            if (value >= rule_range[0][0] and value <= rule_range[0][1]) \
                    or (value >= rule_range[1][0] and value <= rule_range[1][1]):
                    if rule_name in res:
                        res[rule_name].add(i)
                    else:
                        res[rule_name] = {i}
    return res


def map_fields(tickets, rules):
    """
    Map rule fields to ticket value index
    return:
        dict(rule_name: index)
    """
    rulename_to_index = {}
    for ticket in tickets:
        for name, indeces in rulename_to_valid_ticket_index(ticket, rules).items():
            if name in rulename_to_index:
                rulename_to_index[name] = rulename_to_index[name].intersection(indeces)
            else:
                rulename_to_index[name] = indeces
    for k, v in rulename_to_index.items():
        print("{}, {}".format(k, v))


if __name__ == "__main__":
    input_filename = os.path.join(pathlib.Path(__file__).parent, "input.txt")
    with open(input_filename, "r") as input_file:
        rules, your_ticket, nearby_tickets = parse_input(input_file)
    print(scanning_error_rate(nearby_tickets, rules))
    valid_tickets = remove_invalid_tickets(nearby_tickets, rules)
    map_fields(valid_tickets, rules)

    """
    Parsed the rest by hand:
    departure location, 10
    departure station, 17
    departure platform, 12
    departure track, 15
    departure date, 6
    departure time, 11
    arrival location, 3
    arrival station, 0
    arrival platform, 13
    arrival track, 8
    class, 18
    duration, 16
    price, 14
    route, 5
    row, 1
    seat, 9
    train, 19
    type, 4
    wagon, 2
    zone, 7

    0, 1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19
    """
