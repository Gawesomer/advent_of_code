def validate_byr(byr):
    """
    Validate birth year
    params:
        byr (str): birth year
    returns:
        True if `byr` is a valid year within [1920, 2002], False otherwise
    """
    try:
        int_byr = int(byr)
    except ValueError:
        return False
    return (1920 <= int_byr and int_byr <= 2002)


def validate_iyr(iyr):
    """
    Validate issue year
    params:
        iyr (str): issue year
    returns:
        True if `iyr` is a valid year within [2010, 2020], False otherwise
    """
    try:
        int_iyr = int(iyr)
    except ValueError:
        return False
    return (2010 <= int_iyr and int_iyr <= 2020)


def validate_eyr(eyr):
    """
    Validate expiry year
    params:
        eyr (str): expiry year
    returns:
        True if `eyr` is a valid year within [2020, 2030], False otherwise
    """
    try:
        int_eyr = int(eyr)
    except ValueError:
        return False
    return (2020 <= int_eyr and int_eyr <= 2030)


def validate_hgt(hgt):
    """
    Validate height
    params:
        hgt (str): height. must be in the format "{number}{unit}", where
                   `number` is an integer and `unit` is one of {"cm, "in"}
    returns:
        True if `hgt` is formatted properly and `number` is within [150, 193]
        if `unit` is "cm", or within [59, 76] if `unit` is "in",
        False otherwise
    """
    num = hgt[:len(hgt)-2]
    try:
        int_num = int(num)
    except ValueError:
        return False
    if hgt[-2:] == "cm":
        return (150 <= int_num and int_num <= 193)
    elif hgt[-2:] == "in":
        return (59 <= int_num and int_num <= 76)
    return False


def validate_hcl(hcl):
    """
    Validate hair color
    params:
        hcl (str): hair color. must be in the format "#%x%x%x%x%x%x%x", where
        "%x" is a digit or a letter within ['a', 'f']
    returns:
        True if `hcl` is formatted properly, False otherwise
    """
    if hcl[0] != '#':
        return False
    if len(hcl) > 7:
        return False
    for i in range(1, 7):
        if not (('a' <= hcl[i] and hcl[i] <= 'f')
                or ('0' <= hcl[i] and hcl[i] <= '9')):
            return False
    return True


def validate_ecl(ecl):
    """
    Validate eye color
    params:
        ecl (str): eye color. must be one of
                   {"amb", "blu", "brn", "gry", "grn", "hzl", "oth"}
    returns:
        True if `ecl` is a valid color, False otherwise
    """
    if ecl == "amb":
        return True
    elif ecl == "blu":
        return True
    elif ecl == "brn":
        return True
    elif ecl == "gry":
        return True
    elif ecl == "grn":
        return True
    elif ecl == "hzl":
        return True
    elif ecl == "oth":
        return True
    return False


def validate_pid(pid):
    """
    Validate passport ID
    params:
        pid (str): passport ID. must be a nine digit integer, including leading
                   zeroes
    returns:
        True if `pid` is a valid passport ID, False otherwise
    """
    if len(pid) != 9:
        return False
    try:
        pid = int(pid)
    except ValueError:
        return False
    return True
