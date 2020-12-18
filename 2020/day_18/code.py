import os
import pathlib


def parse_input(input_file):
    """
    Get expressions from file
    params:
        input_file - file to parse, opened for reading
    returns:
        list of expressions
        bitmask are stored in tuples
        instructions are stored in lists
    """
    res = []
    for line in input_file:
        res.append(line.replace(' ', '').strip())
    return res


def operation(a, b, op):
    if op == '*':
        return int(a)*int(b)
    elif op == '+':
        return int(a)+int(b)


def matching_closing(expr, index):
    if expr[index] != '(':
        return None
    stck_count = 0
    i = index+1
    while i < len(expr):
        if expr[i] == '(':
            stck_count += 1
        elif expr[i] == ')':
            if stck_count == 0:
                return i
            stck_count -= 1
        i += 1
    return None


def matching_open(expr, index):
    if expr[index] != ')':
        return None
    stck_count = 0
    i = index-1
    while i >= 0:
        if expr[i] == ')':
            stck_count += 1
        elif expr[i] == '(':
            if stck_count == 0:
                return i
            stck_count -= 1
        i -= 1
    return None


def evaluate_expression(expr):
    """
    return:
        integer result
    """
    if not expr:
        return 0
    if expr[0] == '(':
        closing_index = matching_closing(expr, 0)
        if closing_index == len(expr)-1:
            return evaluate_expression(expr[1:closing_index])
        return operation(evaluate_expression(expr[1:closing_index]), evaluate_expression(expr[closing_index+2:]), expr[closing_index+1])
    if len(expr) == 1:
        return int(expr[0])
    return operation(expr[0], evaluate_expression(expr[2:]), expr[1])


def add_first_parentheses(expr):
    expr_len = len(expr)
    i = 0
    while i < expr_len:
        if expr[i] == '+':
            if expr[i-1] == ')':
                open_index = matching_open(expr, i-1)
                expr = expr[0:open_index]+'('+expr[open_index:]
            else:
                expr = expr[0:i-1]+'('+expr[i-1:]
            i += 1
            expr_len += 1

            if expr[i+1] == '(':
                close_index = matching_closing(expr, i+1)
                expr = expr[0:close_index+1]+')'+expr[close_index+1:]
            else:
                expr = expr[0:i+2]+')'+expr[i+2:]
            i += 1
            expr_len += 1
        i += 1
    return expr

        
def reverse_expr(expr):
    revrs = expr.replace('(', 't')
    revrs = revrs.replace(')', '(')
    revrs = revrs.replace('t', ')')
    revrs = revrs[len(revrs)::-1]
    return revrs


if __name__ == "__main__":
    input_filename = os.path.join(pathlib.Path(__file__).parent, "input.txt")
    with open(input_filename, "r") as input_file:
        expressions = parse_input(input_file)
    res = 0
    for expr in expressions:
        expr = reverse_expr(expr)
        res += evaluate_expression(expr)
    print(res)
    res = 0
    for expr in expressions:
        expr = add_first_parentheses(expr)
        expr = reverse_expr(expr)
        res += evaluate_expression(expr)
    print(res)
