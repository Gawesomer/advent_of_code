import os
import pathlib
import sys


def parse_input(input_file):
    """
    Get expressions from file
    params:
        input_file - file to parse, opened for reading
    returns:
        list of expressions
        bitmask are stored in tuples
        instructions are stored in lists
    """
    rules = {}
    messages = []
    parsing_rules = True
    i = 0

    for line in input_file:
        if line == '\n':
            parsing_rules = False
        else:
            if parsing_rules:
                rule_index = int(line[:line.find(':')])
                if '"' not in line:
                    rules[rule_index] = []
                    for or_rule in line[line.find(':')+1:].strip().split('|'):
                        rules[rule_index].append([int(e) for e in or_rule.strip().split(' ')])
                else:
                    rules[rule_index] = line[line.find(':')+3]
                i += 1
            else:
                messages.append(line.strip())
    return rules, messages


def matches_rule(msg, rules, rule):
    """
    return:
        True if msg matches rule number rule_num
    """
    if isinstance(rule, str):
        if msg and msg[0] == rule:
            return msg[0], msg[1:]
        return '', msg
    
    for sub_rule in rule:
        match = ''
        rest = msg
        for r_index in sub_rule:
            sub_match, rest = matches_rule(rest, rules, rules[r_index])
            match += sub_match
        if not rest:
            return match, ''
    return '', msg


def get_all_sub_indeces(rule):
    if isinstance(rule, str):
        return set()

    sub_indeces = set()
    for sub_rule in rule:
        for index in sub_rule:
            sub_indeces.add(index)
    return sub_indeces


def combine_sets(s1, s2):
    """
    {'a', 'b'}, {'b', 'a'} -> {'ab', 'aa', 'bb', 'ba'}
    """
    combined = set()
    for e1 in s1:
        for e2 in s2:
            combined.add(e1+e2)
    return combined


def compile_subrule(rule, processed):
    """
    Takes subrule returns set of acceptable strings
    """
    compiled = []
    for index in rule:
        compiled.append(processed[index])
    while len(compiled) > 1:
        new_set = combine_sets(compiled.pop(0), compiled.pop(0))
        compiled.insert(0, new_set)
    return compiled[0]


def compile_rule(rule, processed):
    """
    Takes rule returns list of sets of acceptable strings
    """
    compiled = set()
    for sub_rule in rule:
        new_set = compile_subrule(sub_rule, processed)
        for e in new_set:
            compiled.add(e)
    return compiled


def process_rules(rules):
    processed_rules = {i: set(e) for i, e in rules.items() if isinstance(e, str)}
    rule_to_sub_index_set = {}

    for i, r in rules.items():
        rule_to_sub_index_set[i] = get_all_sub_indeces(r)

    left_to_compile = True
    while left_to_compile:
        print(processed_rules.keys())
        left_to_compile = False
        for i, r in rules.items():
            if i not in processed_rules:
                if rule_to_sub_index_set[i].issubset(set(processed_rules.keys())):
                    processed_rules[i] = compile_rule(r, processed_rules)
                left_to_compile = True
    return processed_rules


def remove_match(msg, patterns):
    for p in patterns:
        if msg.startswith(p):
            return True, msg[len(p):]
    return False, msg


def split_matches(msgs, patterns):
    matches = []
    non_matches = []
    for msg in msgs:
        matching, rest = remove_match(msg, patterns)
        if matching:
            matches.append(rest)
        else:
            non_matches.append(rest)
    return matches, non_matches


if __name__ == "__main__":
    input_filename = os.path.join(pathlib.Path(__file__).parent, "input.txt")
    with open(input_filename, "r") as input_file:
        rules, messages = parse_input(input_file)
    processed = process_rules(rules)
    matched = []
    for msg in messages:
        if msg in processed[0]:
            matched.append(msg)
    print(len(matched))

    unmatched = [m for m in messages if m not in matched]
    potential = []
    for msg in unmatched:
        finding = True
        found_count = 0
        while finding:
            finding, msg = remove_match(msg, processed[42])
            if finding:
                found_count += 1
        if found_count > 1 and msg:
            potential.append([msg, found_count])
    for msg, found_count in potential:
        finding = True
        new_pattern_found_count = 0
        while finding:
            finding, msg = remove_match(msg, processed[31])
            if finding:
                new_pattern_found_count += 1
        if new_pattern_found_count > 0 and not msg:
            if new_pattern_found_count < found_count:
                matched.append(msg)
    print(len(matched))
