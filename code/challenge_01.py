
def get_numbers_from_file(input_file):
    """
    Gets integers from a file
    Assumes that each line of the file contains a single integer
    params:
        input_file - file to parse, opened for reading
    returns:
        set of integers in file
    """
    res = set()
    for line in input_file:
        res.add(int(line))
    return res


def get_pair_that_sums_to(nums, target):
    """
    Returns pair of numbers in `nums` that sums to `target`
    params:
        nums - set of integers
        target - integer
    returns:
        (a, b) such that a+b=target,
        returns the first matching pair if multiple exist,
        or (None, None) if no such pair exists
    """
    for num in nums:
        if (target-num) in nums:
            return (num, target-num)
    return (None, None)


if __name__ == "__main__":
    with open("input_file.txt", "r") as input_file:
        nums = get_numbers_from_file(input_file)
    a, b = get_pair_that_sums_to(nums, 2020)
    print(a*b)
