def parse_line_from_file(input_file):
    """
    Get lines from file
    params:
        input_file - file to parse, opened for reading
    returns:
        collection of strings
    """
    res = []
    for line in input_file:
        res.append(line)
    return res


def password_is_valid(policy_password):
    """
    Returns whether or not given password is valid
    Assumes password is a string formatted as such:
        "min-max letter: password"
    params:
        password - string
    returns:
        True if `letter` appears at least `min` times and at most `max` times
        in `password`, False otherwise
    """
    policy, password = policy_password.split(':')
    min_max, letter = policy.split(' ')
    min_count, max_count = min_max.split('-')
    min_count = int(min_count)
    max_count = int(max_count)

    letter_count = password.count(letter)

    return (min_count <= letter_count and letter_count <= max_count)


def num_invalid_passwords(passwords):
    """
    Return number of invalid passwords
    Assumes each string given consists of a password policy and corresponding
    password formatted as such:
        "min-max letter: password"
    params:
        passwords - collection of strings
    returns:
        number of passwords in input that do not match their policy
    """
    res = 0
    for line in passwords:
        if password_is_valid(line):
            res += 1
    return res


if __name__ == "__main__":
    with open("input_02.txt", "r") as input_file:
        lines = parse_line_from_file(input_file)
    print(num_invalid_passwords(lines))
