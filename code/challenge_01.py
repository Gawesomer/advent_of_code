
def get_numbers_from_file(input_file):
    """
    Gets integers from a file
    Assumes that each line of the file contains a single integer
    params:
        input_file - file to parse, opened for reading
    returns:
        set of integers in file
    """
    res = []
    for line in input_file:
        res.append(int(line))
    return res


def get_x_numbers_that_sum_to_target(nums, x, target):
    """
    Returns x numbers in `nums` that sum to `target`
    params:
        nums - set of integers
        x - number of numbers that should be summed
        target - integer
    returns:
        (a, b, ...) such that a+b+...=target,
        returns the first matching list if multiple exist,
        or None if no such lists exists
    """
    res = []
    if x <= 1:
        if target in nums:
            res.append(target)
        return res

    for i in range(len(nums)):
        tmp = nums[i]
        del nums[i]
        ret = get_x_numbers_that_sum_to_target(nums, x-1, target-tmp)
        if len(ret) > 0:
            ret.append(tmp)
            return ret
        nums.append(tmp)
    return []



if __name__ == "__main__":
    with open("input_file.txt", "r") as input_file:
        nums = get_numbers_from_file(input_file)
    a, b, c = get_x_numbers_that_sum_to_target(nums, 3, 2020)
    print(a*b*c)
