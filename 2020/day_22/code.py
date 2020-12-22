import os
import pathlib


def parse_input(input_file):
    """
    Get decks from file
    params:
        input_file - file to parse, opened for reading
    returns:
        list(int), list(int)
    """
    stck1 = []
    stck2 = []
    first_stck = True
    for line in input_file:
        if line.startswith("Player"):
            continue
        if line == '\n':
            first_stck = False
            continue
        if first_stck:
            stck1.append(int(line.strip()))
        else:
            stck2.append(int(line.strip()))
    return stck1, stck2


def combat_round(stck1, stck2):
    top1 = stck1.pop(0)
    top2 = stck2.pop(0)

    if top1 > top2:
        stck1.append(top1)
        stck1.append(top2)
    else:
        stck2.append(top2)
        stck2.append(top1)


def add_winnings(stck, winner_card, loser_card):
    stck.append(winner_card)
    stck.append(loser_card)


def recursive_combat(stck1, stck2):
    """
    returns:
        True if player1 won, False otherwise
    """
    mem = []
    print("combat: {}; {}".format(stck1, stck2))
    while len(stck1) > 0 and len(stck2) > 0:
        for m in mem:
            if stck1 == m[0] and stck2 == m[1]:
                return True
        mem.append((stck1.copy(), stck2.copy()))
        top1 = stck1.pop(0)
        top2 = stck2.pop(0)

        if len(stck1) < top1 or len(stck2) < top2:
            if top1 > top2:
                add_winnings(stck1, top1, top2)
                continue
            else:
                add_winnings(stck2, top2, top1)
                continue

        stck1_copy = [e for e in stck1[:top1]]
        stck2_copy = [e for e in stck2[:top2]]

        if recursive_combat(stck1_copy, stck2_copy):
            add_winnings(stck1, top1, top2)
        else:
            add_winnings(stck2, top2, top1)
    return (len(stck1) > 0)


def count_score(stck):
    res = 0
    i = len(stck)-1
    multiplier = 1
    while i >= 0:
        res += stck[i] * multiplier
        multiplier += 1
        i -= 1
    return res


if __name__ == "__main__":
    input_filename = os.path.join(pathlib.Path(__file__).parent, "input.txt")
    with open(input_filename, "r") as input_file:
        stck1, stck2 = parse_input(input_file)

    """
    while len(stck1) > 0 and len(stck2) > 0:
        combat_round(stck1, stck2)

    if len(stck1) == 0:
        print(count_score(stck2))
    else:
        print(count_score(stck1))
    """
    if recursive_combat(stck1, stck2):
        print(count_score(stck1))
    else:
        print(count_score(stck2))
