import os
import pathlib


def parse_numbers(input_file):
    """
    Get numbers from file
    params:
        input_file - file to parse, opened for reading
    returns:
        list(int) numbers from file
    """
    res = []
    for line in input_file:
        res.append(int(line))
    return res


def find_joltage_differences(adapters):
    """
    Finds joltage differences between adapters
    params:
        list(int): from `parse_numbers`
    returns:
        list(int) index i contains number of i-jold differences
    """
    res = [0, 0, 0]
    sorted_adapters = sorted(adapters)
    prev = 0    # Charging outlet with rating of 0
    for adapter in sorted_adapters:
        res[adapter-prev-1] += 1
        prev = adapter
    res[2] += 1 # Device rated 3-jolts higher than max
    return res


def find_joltage_arrangements(adapters):
    """
    Finds number of adapter arrangements from `start` to `end`
    params:
        adapters (list(int)) - sorted containing 0 and device rating
    return:
        int
    """
    num_adapters = len(adapters)
    memo = [0 for i in range(0, num_adapters)]
    memo[num_adapters-1] = 1
    memo[num_adapters-2] = 1
    memo[num_adapters-3] = 1
    if adapters[num_adapters-1]-adapters[num_adapters-3] <= 3:
        memo[num_adapters-3] += 1
    i = num_adapters-4
    while i >= 0:
        if adapters[i+1]-adapters[i] <= 3:
            memo[i] += memo[i+1]
        if adapters[i+2]-adapters[i] <= 3:
            memo[i] += memo[i+2]
        if adapters[i+3]-adapters[i] <= 3:
            memo[i] += memo[i+3]
        i -= 1
    return memo[0]


if __name__ == "__main__":
    input_filename = os.path.join(pathlib.Path(__file__).parent, "input.txt")
    with open(input_filename, "r") as input_file:
        nums = parse_numbers(input_file)
    jolt_diffs = find_joltage_differences(nums)
    print(jolt_diffs[0]*jolt_diffs[2])
    nums.append(0)
    nums.append(max(nums)+3)
    print(find_joltage_arrangements(sorted(nums)))
