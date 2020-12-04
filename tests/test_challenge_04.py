import io
import unittest

from code.challenge_04 import *


class TestParser(unittest.TestCase):

    def setUp(self):
        pass

    def test_parse_passports_nominal_case(self):
        s = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\n" \
            "byr:1937 iyr:2017 cid:147 hgt:183cm\n" \
            "\n" \
            "hcl:#ae17e1 iyr:2013\n" \
            "eyr:2024\n" \
            "ecl:brn pid:760753108 byr:1931\n" \
            "hgt:179cm\n"
        input_file = io.StringIO(s)
        expected_passports = [
            {
                "ecl": "gry",
                "pid": "860033327",
                "eyr": "2020",
                "hcl": "#fffffd",
                "byr": "1937",
                "iyr": "2017",
                "cid": "147",
                "hgt": "183cm",
            },
            {
                "ecl": "brn",
                "pid": "760753108",
                "eyr": "2024",
                "hcl": "#ae17e1",
                "byr": "1931",
                "iyr": "2013",
                "hgt": "179cm",
            },
        ]

        actual_passports = parse_passports(input_file)

        self.assertEqual(expected_passports, actual_passports)


class TestProcessor(unittest.TestCase):

    def setUp(self):
        pass

    def test_validate_passport_all_fields_present(self):
        passport = {
            "ecl": "gry",
            "pid": "860033327",
            "eyr": "2020",
            "hcl": "#fffffd",
            "byr": "1937",
            "iyr": "2017",
            "cid": "147",
            "hgt": "183cm",
        }
        
        self.assertTrue(validate_passport(passport))

    def test_validate_passport_optional_field_missing(self):
        passport = {
            "ecl": "brn",
            "pid": "760753108",
            "eyr": "2024",
            "hcl": "#ae17e1",
            "byr": "1931",
            "iyr": "2013",
            "hgt": "179cm",
        }

        self.assertTrue(validate_passport(passport))

    def test_validate_passport_with_missing_required_field(self):
        passport = {
            "pid": "860033327",
            "eyr": "2020",
            "hcl": "#fffffd",
            "byr": "1937",
            "iyr": "2017",
            "cid": "147",
            "hgt": "183cm",
        }
        
        self.assertFalse(validate_passport(passport))


if __name__ == "__main__":
    unittest.main()
