import io
import unittest

from day_14.code import *


class TestParser(unittest.TestCase):

    def setUp(self):
        pass

    def test_parse_code_nominal_case(self):
        s = "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\n" \
            "mem[8] = 11\n" \
            "mem[7] = 101\n" \
            "mem[8] = 0\n"
        input_file = io.StringIO(s)
        expected_bitmask = (64, 2)
        expected_instructions = [
            [8, 11],
            [7, 101],
            [8, 0],
        ]

        code = parse_input(input_file)
        actual_bitmask = code[0]
        actual_instructions = code[1:4]

        self.assertEqual(expected_bitmask, actual_bitmask)
        self.assertEqual(expected_instructions, actual_instructions)


class TestProcessor(unittest.TestCase):

    def setUp(self):
        pass

    def test_execute_nominal_case(self):
        code = [
            (64, 2),
            [8, 11],
            [7, 101],
            [8, 0],
        ]

        self.assertEqual(execute(code), 165)



if __name__ == "__main__":
    unittest.main()
