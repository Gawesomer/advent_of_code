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

        self.assertEqual(expected_bitmask, actual_bitmask[0:2])
        self.assertEqual(expected_instructions, actual_instructions)

    def test_parse_input_for_decoder_nominal_case(self):
        s = "mask = 000000000000000000000000000000X1001X\n" \
            "mem[42] = 100\n" \
            "mask = 00000000000000000000000000000000X0XX\n" \
            "mem[26] = 1\n"
        input_file = io.StringIO(s)
        expected_code = [
            (18, 33),
            [42, 100],
            (0, 11),
            [26, 1],
        ]

        code = parse_input(input_file, 'X')

        self.assertEqual(expected_code, code)



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

    def test_floating_permutation_nominal_case(self):
        expected_permutations = [0, 1, 32, 33]

        self.assertEqual(sorted(floating_permutations(33)), expected_permutations)

    def test_execute_address_decoder_nominal_case(self):
        code = [
            (18, 33),
            [42, 100],
            (0, 11),
            [26, 1],
        ]

        self.assertEqual(execute_memory_address_decoder(code), 208)


if __name__ == "__main__":
    unittest.main()
