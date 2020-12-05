def parse_passports(input_file):
    """
    Get passports from file
    params:
        input_file - file to parse, opened for reading
    returns:
        collection of passport dictionaries
    """
    res = []
    curr_dict = {}
    for line in input_file:
        if line.strip() == "":
            res.append(curr_dict)
            curr_dict = {}
        else:
            for pair in line.split():
                key, value = pair.split(':')
                curr_dict[key] = value
    res.append(curr_dict)
    return res


def validate_byr(byr):
    try:
        int_byr = int(byr)
    except ValueError:
        return False
    return (1920 <= int_byr and int_byr <= 2002)


def validate_iyr(iyr):
    try:
        int_iyr = int(iyr)
    except ValueError:
        return False
    return (2010 <= int_iyr and int_iyr <= 2020)


def validate_eyr(eyr):
    try:
        int_eyr = int(eyr)
    except ValueError:
        return False
    return (2020 <= int_eyr and int_eyr <= 2030)


def validate_hgt(hgt):
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
    if hcl[0] != '#':
        return False
    if len(hcl) > 7:
        return False
    for i in range(1, 7):
        if not (('a' <= hcl[i] and hcl[i] <= 'f') or ('0' <= hcl[i] and hcl[i] <= '9')):
            return False
    return True


def validate_ecl(ecl):
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
    if len(pid) != 9:
        return False
    try:
        pid = int(pid)
    except ValueError:
        return False
    return True


def validate_passport(passport):
    """
    Validates that passport has all required fields:
        byr (Birth Year)
        iyr (Issue Year)
        eyr (Expiration Year)
        hgt (Height)
        hcl (Hair Color)
        ecl (Eye Color)
        pid (Passport ID)
        cid (Country ID) - Optional
    params:
        passport - dictionary
    returns:
        True if `passport` contains all required fields, False otherwise
    """
    field_validators = {
        "byr": validate_byr,
        "iyr": validate_iyr,
        "eyr": validate_eyr,
        "hgt": validate_hgt,
        "hcl": validate_hcl,
        "ecl": validate_ecl,
        "pid": validate_pid,
    }
    required_fields = {
        "byr": False,
        "iyr": False,
        "eyr": False,
        "hgt": False,
        "hcl": False,
        "ecl": False,
        "pid": False,
    }
    optional_fields = {
        "cid": False,
    }
    for key, value in passport.items():
        if key in required_fields:
            if required_fields[key] == False:
                required_fields[key] = field_validators[key](value)
            else:
                return False
        elif key in optional_fields:
            if optional_fields[key] == False:
                optional_fields[key] = True
            else:
                return False
    return all(required_fields.values())


if __name__ == "__main__":
    with open("input_04.txt", "r") as input_file:
        passports = parse_passports(input_file)
    valid_count = 0
    for passport in passports:
        if validate_passport(passport):
            valid_count += 1
    print(valid_count)
