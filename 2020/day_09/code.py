import os
import pathlib
import sys

from day_01.code import get_n_numbers_that_sum_to_target


def parse_numbers(input_file):
    """
    Get numbers from file
    params:
        input_file - file to parse, opened for reading
    returns:
        list(int)
    """
    res = []
    for line in input_file:
        res.append(int(line))
    return res


def is_pair_sum(nums, target):
    """
    Determine if a pair of distinct elements in `nums` can sum to `target`
    params:
        nums (list(int)): numbers to use
        target (int): target to sum to
    returns:
        True if pair of distinct integers in `nums` sums to `target`,
        False otherwise
    """
    num_set = set(nums)
    for num in num_set:
        if ((target-num) in num_set) and (target-num != num):
            return True
    return False


def find_first_invalid(nums, n):
    """
    Find first number that isn't sum of a distinct pair of previous `n` numbers
    params:
        nums (list): result of `parse_numbers`
        n (int): preamble length
    returns:
        first invalid number
    """
    prev_n = []
    i = 0
    while i < n:
        prev_n.append(nums[i])
        i += 1
    while i < len(nums):
        if not is_pair_sum(prev_n, nums[i]):
            return nums[i]
        prev_n.pop(0)
        prev_n.append(nums[i])
        i += 1
    return None


def find_contiguous_sum(nums, target):
    """
    return:
        start, end: integer indeces of the contiguous array that sums to target
    """
    i = 0
    while i < len(nums):
        curr_sum = 0
        j = i
        while curr_sum < target:
            curr_sum += nums[j]
            j += 1
        if curr_sum == target:
            return i, j-1
        i += 1
    return None, None


if __name__ == "__main__":
    input_filename = os.path.join(pathlib.Path(__file__).parent, "input.txt")
    with open(input_filename, "r") as input_file:
        nums = parse_numbers(input_file)
    print(find_first_invalid(nums, 25))
    target = 29221323
    start, end = find_contiguous_sum(nums, target)
    min_range = sys.maxsize
    max_range = 0
    i = start
    while i <= end:
        if nums[i] < min_range:
            min_range = nums[i]
        if nums[i] > max_range:
            max_range = nums[i]
        i += 1
    print(max_range+min_range)
