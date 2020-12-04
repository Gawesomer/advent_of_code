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
                required_fields[key] = True
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
