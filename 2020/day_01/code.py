import pathlib
import os


def get_numbers_from_file(input_file):
    """
    Get integers from file
    Assumes that each line of the file contains a single integer
    params:
        input_file - file to parse, opened for reading
    returns:
        collection of integers in file
    """
    res = []
    for line in input_file:
        res.append(int(line))
    return res


def get_n_numbers_that_sum_to_target(nums, n, target):
    """
    Returns `n` numbers in `nums` that sum to `target`
    params:
        nums - collection of integers
        n - number of numbers that should be summed
        target - integer
    returns:
        [x_1, x_2, ..., x_n] such that x_1+x_2+...+x_n=target,
        returns the first matching list found if multiple exist,
        or an empty list if no such list exists
    """
    res = []
    if n <= 1:
        if target in nums:
            res.append(target)
        return res

    for i in range(len(nums)-(n-1)):
        tmp = nums[i]
        del nums[i]
        ret = get_n_numbers_that_sum_to_target(nums, n-1, target-tmp)
        if len(ret) > 0:
            ret.append(tmp)
            return ret
        nums.insert(i, tmp)
    return []


if __name__ == "__main__":
    input_filename = os.path.join(pathlib.Path(__file__).parent, "input.txt")
    with open(input_filename, "r") as input_file:
        nums = get_numbers_from_file(input_file)
    nums = get_n_numbers_that_sum_to_target(nums, 3, 2020)
    res = 1
    for num in nums:
        res *= num
    print(res)
