import io
import unittest

from day_08.code import *


class TestParser(unittest.TestCase):

    def setUp(self):
        pass

    def test_parse_code_nominal_case(self):
        s = "nop +0\n" \
            "acc +1\n" \
            "jmp +4\n" \
            "acc +3\n" \
            "jmp -3\n" \
            "acc -99\n" \
            "acc +1\n" \
            "jmp -4\n" \
            "acc +6\n"
        input_file = io.StringIO(s)
        expected_code = [
            ["nop", 0],
            ["acc", 1],
            ["jmp", 4],
            ["acc", 3],
            ["jmp", -3],
            ["acc", -99],
            ["acc", 1],
            ["jmp", -4],
            ["acc", 6],
        ]

        actual_code = parse_code(input_file)

        self.assertEqual(expected_code, actual_code)


class TestProcessor(unittest.TestCase):

    def setUp(self):
        pass

    def test_run_code_nominal_case(self):
        code = [
            ["nop", 0],
            ["acc", 1],
            ["jmp", 4],
            ["acc", 3],
            ["jmp", -3],
            ["acc", -99],
            ["acc", 1],
            ["jmp", -4],
            ["acc", 6],
        ]

        self.assertEqual(run_code(code), (False, 5))

    def test_run_code_case_without_loop(self):
        code = [
            ["nop", 0],
            ["acc", 1],
            ["jmp", 4],
            ["acc", 3],
            ["jmp", -3],
            ["acc", -99],
            ["acc", 1],
            ["nop", -4],
            ["acc", 6],
        ]

        self.assertEqual(run_code(code), (True, 8))


if __name__ == "__main__":
    unittest.main()
