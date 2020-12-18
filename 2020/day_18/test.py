import io
import unittest

from day_18.code import *


class TestParser(unittest.TestCase):

    def setUp(self):
        pass

    def test_parse_code_nominal_case(self):
        s = "1 + 2 * 3 + 4 * 5 + 6\n" \
            "1 + (2 * 3) + (4 * (5 + 6))\n"
        input_file = io.StringIO(s)
        expected_expressions = [
            "1+2*3+4*5+6",
            "1+(2*3)+(4*(5+6))",
        ]

        expressions = parse_input(input_file)
        self.assertEqual(expected_expressions, expressions)


class TestProcessor(unittest.TestCase):

    def setUp(self):
        pass

    def test_evaluate_expression_single_integer(self):
        expr = '1'
        self.assertEqual(evaluate_expression(expr), 1)

    def test_evaluate_expression_single_addition(self):
        expr = '1+2'
        self.assertEqual(evaluate_expression(expr), 3)

    def test_evaluate_expression_single_multiplication(self):
        expr = '2*2'
        self.assertEqual(evaluate_expression(expr), 4)

    def test_evaluate_expression_single_expression_parentheses(self):
        expr = '(1+2)'
        self.assertEqual(evaluate_expression(expr), 3)

    def test_evaluate_expression_two_expression(self):
        expr = '1+2*3'
        expr = expr[len(expr)::-1]
        self.assertEqual(evaluate_expression(expr), 9)

    def test_evaluate_expression_without_parentheses_nominal_case(self):
        expr = '1+2*3+4*5+6'
        expr = reverse_expr(expr)
        self.assertEqual(evaluate_expression(expr), 71)

    def test_evaluate_expression_with_parentheses_nominal_case(self):
        expr = "1+(2*3)+(4*(5+6))"
        expr = reverse_expr(expr)
        self.assertEqual(evaluate_expression(expr), 51)

    def test_add_first_parentheses_no_addition(self):
        expr = "1*2"
        expected = expr

        expr = add_first_parentheses(expr)

        self.assertEqual(expected, expr)

    def test_add_first_parentheses_single_addition(self):
        expr = "1+2"
        expected = "(1+2)"

        expr = add_first_parentheses(expr)

        self.assertEqual(expr, expected)

    def test_add_first_parentheses_brackets_on_left(self):
        expr = "(1*2)+3*2"
        expected = "((1*2)+3)*2"

        expr = add_first_parentheses(expr)

        self.assertEqual(expr, expected)

    def test_add_first_parentheses_brackets_on_right(self):
        expr = "2*3+(2*1)"
        expected = "2*(3+(2*1))"

        expr = add_first_parentheses(expr)

        self.assertEqual(expr, expected)

    def test_add_first_parentheses_brackets_nominal_case(self):
        expr = "((2+4*9)*(6+9*8+6)+6)+2+4*2"
        expr = add_first_parentheses(expr)
        expr = reverse_expr(expr)

        self.assertEqual(evaluate_expression(expr), 23340)


if __name__ == "__main__":
    unittest.main()
