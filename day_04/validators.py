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
