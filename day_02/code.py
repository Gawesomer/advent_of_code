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


def parse_password_policy(policy_password):
    """
    Parse and return information from password_policy
    Assumes `policy_password` is a string formatted as such:
        "min-max letter: password"
    params:
        policy_password - string
    returns:
        (min, max, letter, password)
        min - integer
        max - integer
        letter - string
        password - string
    """
    policy, password = policy_password.split(':')
    password = password[1:]
    min_max, letter = policy.split(' ')
    min_count, max_count = min_max.split('-')
    min_count = int(min_count)
    max_count = int(max_count)

    return (min_count, max_count, letter, password)


def count_password_policy(policy_password):
    """
    Determine whether password is valid
    Assumes `policy_password` is a string formatted as such:
        "min-max letter: password"
    params:
        policy_password - string
    returns:
        True if `letter` appears at least `min` times and at most `max` times
        in `password`, False otherwise
    """
    min_count, max_count, letter, password = parse_password_policy(
                                                policy_password)

    letter_count = password.count(letter)

    return (min_count <= letter_count and letter_count <= max_count)

def position_password_policy(policy_password):
    """
    Determine wheter password is valid
    Assumes `policy_password` is a string formatted as such:
        "min-max letter: password"
    params:
        policy_password - string
    returns:
        True if `letter` appears at position `min` xor at position `max` in
        `password`, False otherwise
        (Note: position is determined using 1-indexing)
    """
    min_count, max_count, letter, password = parse_password_policy(
                                                policy_password)

    if len(password) < min_count:
        return False
    if password[min_count-1] == letter and password[max_count-1] == letter:
        return False
    if password[min_count-1] == letter:
        return True
    if password[max_count-1] == letter:
        return True
    return False


def num_invalid_passwords(policy, policy_passwords):
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
    for line in policy_passwords:
        if policy(line):
            res += 1
    return res


if __name__ == "__main__":
    with open("input.txt", "r") as input_file:
        lines = parse_line_from_file(input_file)
    print(num_invalid_passwords(position_password_policy, lines))
