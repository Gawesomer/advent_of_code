import os
import pathlib

# Part1

def insert_mod(i, elements, l, n):
    for e in elements:
        l.insert(i%n, e)
        i += 1


def find_index(element, l):
    for i, e in enumerate(l):
        if e == element:
            return i
    return None


def step(cups, curr_val):
    curr_index = find_index(curr_val, cups)
    cup_len = len(cups)

    removed = []
    pop_index = (curr_index+1)%len(cups)
    removed.append(cups.pop(pop_index))
    if pop_index == len(cups):
        pop_index = 0
    removed.append(cups.pop(pop_index))
    if pop_index == len(cups):
        pop_index = 0
    removed.append(cups.pop(pop_index))

    lowest = min(cups)
    highest = max(cups)

    dest = curr_val-1
    if dest < lowest:
        dest = highest
    else:
        while dest not in cups:
            dest -= 1

    dest_index = find_index(dest, cups)

    insert_mod(dest_index+1, removed, cups, cup_len)

    curr_index = find_index(curr_val, cups)

    return cups[(curr_index+1)%len(cups)]

# end Part1


def serialize(cups):
    """
    return:
        adjacency map: map[i] = cups.find(i-1)+1
    """
    res = [0 for e in cups]

    prev = cups[0]
    i = 1
    while i < len(cups):
        res[prev-1] = cups[i]
        prev = cups[i]
        i += 1
    res[prev-1] = cups[0]

    return res


def next_element(seq_map, e):
    return seq_map[e-1]


def get_nth_element(seq_map, e, offset):
    res = e
    for i in range(offset):
        res = next_element(seq_map, res)
    return res


def shuffle(seq_map, curr, highest):
    to_move = [
        get_nth_element(seq_map, curr, 1),
        get_nth_element(seq_map, curr, 2),
        get_nth_element(seq_map, curr, 3),
    ]

    dest = curr-1
    if dest < 1:
        dest = highest
    while dest in to_move:
        dest -= 1
        if dest < 1:
            dest = highest

    seq_map[curr-1] = seq_map[to_move[2]-1]
    tmp = seq_map[dest-1]
    seq_map[dest-1] = to_move[0]
    seq_map[to_move[2]-1] = tmp

    return seq_map[curr-1]



if __name__ == "__main__":
    cups = [1, 5, 8, 9, 3, 7, 4, 6, 2]
    seq_map = serialize(cups)

    curr = 1
    for i in range(100):
        curr = shuffle(seq_map, curr, 9)
    print(seq_map)

    for i in range(10, 1000001):
        cups.append(i)

    seq_map = serialize(cups)
    curr = 1
    for i in range(10000000):
        curr = shuffle(seq_map, curr, 1000000)
    print("{} * {}".format(seq_map[0], seq_map[seq_map[0]-1]))
