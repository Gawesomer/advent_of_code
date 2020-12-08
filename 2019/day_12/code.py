import os
import pathlib

from day_12.moon import Moon


def parse_moons(input_file):
    """
    Get moons starting positions from file
    params:
        input_file - file to parse, opened for reading
    returns:
        list(Moon) with positions set to data parsed from file
    """
    res = list()
    for line in input_file:
        words = line.split()
        res.append(
            Moon(
                int(words[0][3:-1]),
                int(words[1][2:-1]),
                int(words[2][2:-1]),
                )
        )
    return res


def apply_gravity(moons):
    """
    Apply gravity to `moons`
    params:
        moons (list(Moon)): result of `parse_moons`
    returns:
        nothing
    side-effect:
        pulls all pairs of moons
    """
    num_moons = len(moons)
    for i in range(0, num_moons):
        for j in range(0, num_moons):
            if j != i:
                moons[i].pull(moons[j])


def step(moons, n=1):
    """
    Performs one step of the simulation
    params:
        moons (list(Moon)): result of `parse_moons`
        n (int): number of steps to be simulated
    returns:
        nothing
    side-effect:
        applies gravity between moves and moves moons
    """
    for i in range(0, n):
        apply_gravity(moons)
        for moon in moons:
            moon.move()


if __name__ == "__main__":
    input_filename = os.path.join(pathlib.Path(__file__).parent, "input.txt")
    with open(input_filename, "r") as input_file:
        moons = parse_moons(input_file)
    step(moons, 1000)
    total = 0
    for moon in moons:
        total += moon.total_energy
    print(total)
