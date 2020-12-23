import os
import pathlib


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


if __name__ == "__main__":
    cups = [1, 5, 8, 9, 3, 7, 4, 6, 2]
    curr = 1
    for i in range(100):
        curr = step(cups, curr)
    print(cups)
