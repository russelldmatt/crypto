#!/usr/bin/env python
allow_concat_as_normal_operation = True

import sys
import unittest
from math import log

class Operator():
    def __init__(self, f, str, **kwargs):
        self.f = f
        self.str = str
        self.precedence = kwargs['precedence']
        self.is_concat = kwargs['is_concat']
        self.is_commutative = kwargs['is_commutative']
        self.is_associative = kwargs['is_associative']

    def __repr__(self):
        return self.str

from enum import IntEnum
class Precedence(IntEnum):
    Addition = 1
    Subtraction = 1
    Multiplication = 3
    Division = 3
    Exponentiation = 5
    Concatenation = 6
    Number = 7
    Parens = 8

class Expression():
    @classmethod
    def number(cls, n):
        self = cls()
        self.eval = lambda: n
        self.str = str(n)
        self.is_number = True
        self.is_concatable = True
        self.lowest_precedence = Precedence.Number
        return self

    @classmethod
    def operation(cls, op, left, right):
        self = cls()
        def pass_along_none(f):
            return (lambda a, b: None if a == None or b == None else f(a, b))
        f1 = pass_along_none(op.f)
        
        self.is_concatable = allow_concat_as_normal_operation or (op.is_concat and left.is_concatable and right.is_concatable)
        f = lambda a, b: None if op.is_concat and not self.is_concatable else f1(a, b)
                                                 
        self.eval = lambda: f(left.eval(), right.eval())
        self.left_needs_parens = left.lowest_precedence < op.precedence
        # AFAIK, the valid operators are left associative if not
        # associative, so right side needs parens.  For example:
        # 2 - (4 - 5) != 2 - 4 - 5
        # but (2 - 4) - 5 = 2 - 4 - 5
        self.right_needs_parens = right.lowest_precedence < op.precedence or (right.lowest_precedence == op.precedence and not op.is_associative)

        def side_to_precedence(side, is_left):
            needs_parens = self.left_needs_parens if is_left else self.right_needs_parens
            return Precedence.Parens if needs_parens else side.lowest_precedence
        
        self.lowest_precedence = min(side_to_precedence(left, True), side_to_precedence(right, False), op.precedence)
        def side_to_str(side, is_left):
            needs_parens = self.left_needs_parens if is_left else self.right_needs_parens
            # needs_parens = True
            if needs_parens:
                return "({})".format(side)
            else:
                return str(side)
        
        self.str = "{}{}{}".format(side_to_str(left, True), op, side_to_str(right, False))
        self.is_number = False
        return self

    def eval(self):
        return self.eval()

    def __repr__(self):
        return self.str

    def __eq__(self, other):
        return self.str == other.str

    def __hash__(self):
        return hash(self.str)

def count_of_unique_elements(l):
    from collections import Counter
    return dict(Counter(l))

# A simpler all_partitions function, but worse in that it treats every
# element as unique, even if they aren't.  See unit test of ['a', 'a', 'a'].
def all_partitions_simple(l):
    if l == []: yield [], []
    else:
        x, rest = l[0], l[1:]
        for (left, right) in all_partitions(rest):
            yield left, [x] + right
            yield [x] + left, right

def all_partitions(l):
    def loop(counts):
        if len(counts) == 0: yield [], []
        else:
            (el, count), rest = counts[0], counts[1:]
            for (left, right) in loop(rest):
                for n in range(count + 1):
                    yield n * [el] + left, (count - n) * [el] + right
                    
    counts = sorted(list(count_of_unique_elements(l).iteritems()))
    loop(counts)
    for x in loop(counts):
        yield x

def all_non_empty_partitions(l):
    for left, right in all_partitions(l):
        if left != [] and right != []:
            yield left, right

def all_non_empty_partitions_where_left_is_smaller(l):
    for left, right in all_non_empty_partitions(l):
        if left <= right:
            yield left, right
            
def checked_pow(a, b):
    if a == 0: return 0
    if b * log(abs(a)) > 100: return float('inf')
    else: return pow(a, b)

def concat(a, b):
    if b < 0: return None
    if a == 0: return None
    if not float(a).is_integer() or not float(b).is_integer(): return None
    return int(str(int(a)) + str(int(b)))

# define the valid operators
addition       = Operator(lambda a, b: a + b, ' + ',                                      precedence = Precedence.Addition, is_concat = False, is_commutative = True,  is_associative = True)
subtraction    = Operator(lambda a, b: a - b, ' - ',                                      precedence = Precedence.Subtraction, is_concat = False, is_commutative = False, is_associative = False)
multiplication = Operator(lambda a, b: a * b, ' * ',                                      precedence = Precedence.Multiplication, is_concat = False, is_commutative = True,  is_associative = True)
division       = Operator(lambda a, b: None if b == 0 else a / b, ' / ',                  precedence = Precedence.Division, is_concat = False, is_commutative = False, is_associative = False)
exponentiation = Operator(checked_pow, ' ^ ',                                             precedence = Precedence.Exponentiation, is_concat = False, is_commutative = False, is_associative = False)
concatenation  = Operator(concat, ' concat ' if allow_concat_as_normal_operation else '', precedence = Precedence.Concatenation, is_concat = True,  is_commutative = False, is_associative = True)
all_operators = [ addition, subtraction, multiplication, division, exponentiation, concatenation ]

def generate_all_expressions(numbers, operators):
    if len(numbers) == 1:
        n = numbers[0]
        yield Expression.number(n)
    else:
        for operator in operators:
            f = all_non_empty_partitions_where_left_is_smaller if operator.is_commutative else all_non_empty_partitions
            for left, right in f(numbers):
                for left_expr in all_expressions(left, operators):
                    for right_expr in all_expressions(right, operators):
                        yield Expression.operation(operator, left_expr, right_expr)

def all_expressions(numbers, operators):
    seen = set()
    for expr in generate_all_expressions(numbers, operators):
        if expr not in seen:
            seen.add(expr)
            yield expr

class Tests(unittest.TestCase):
    def test_counter1(self):
        self.assertEqual(count_of_unique_elements(['a', 'a', 'a']), {'a' : 3})
        
    def test_counter2(self):
        self.assertEqual(count_of_unique_elements(['a', 'b', 'b', 'c']), {'a' : 1, 'b' : 2, 'c' : 1})
        
    def test_all_partitions1(self):
        self.assertEqual(list(all_partitions(['a', 'a', 'a'])),
                         [
                             ([              ], ['a', 'a', 'a' ]),
                             (['a'           ], ['a', 'a'      ]),
                             (['a', 'a'      ], ['a'           ]),
                             (['a', 'a', 'a' ], [              ]),
                         ])

    def test_all_partitions_simple(self):
        # all_partitions_simple gives duplicate partitions because it
        # treats every element as unique
        self.assertEqual(list(all_partitions_simple(['a', 'a', 'a'])),
                         [
                             ([              ], ['a', 'a', 'a' ]),
                             (['a'           ], ['a', 'a'      ]),
                             (['a'           ], ['a', 'a'      ]),
                             (['a', 'a'      ], ['a'           ]),
                             (['a', 'a'      ], ['a'           ]),
                             (['a', 'a', 'a' ], [              ]),
                         ])
        
    def test_all_partitions2(self):
        self.assertEqual(list(all_partitions(['a', 'b', 'c'])),
                         [
                             ([              ], ['a', 'b', 'c' ]),
                             (['a'           ], ['b', 'c'      ]),
                             (['b'           ], ['a', 'c'      ]),
                             (['a', 'b'      ], ['c'           ]),
                             (['c'           ], ['a', 'b'      ]),
                             (['a', 'c'      ], ['b'           ]),
                             (['b', 'c'      ], ['a'           ]),
                             (['a', 'b', 'c' ], [              ]),
                         ])
        
    def test_all_non_empty_partitions(self):
        self.assertEqual(list(all_non_empty_partitions(['a', 'b', 'c'])),
                         [
                             (['a'           ], ['b', 'c'      ]),
                             (['b'           ], ['a', 'c'      ]),
                             (['a', 'b'      ], ['c'           ]),
                             (['c'           ], ['a', 'b'      ]),
                             (['a', 'c'      ], ['b'           ]),
                             (['b', 'c'      ], ['a'           ]),
                         ])

    def test_all_non_empty_partitions_where_left_is_smaller1(self):
        self.assertEqual(list(all_non_empty_partitions_where_left_is_smaller(['a', 'b', 'c'])),
                     [
                         (['a'           ], ['b', 'c'      ]),
                         (['a', 'b'      ], ['c'           ]),
                         (['a', 'c'      ], ['b'           ]),
                     ])
        
    def test_all_non_empty_partitions_where_left_is_smaller2(self):
        self.assertEqual(list(all_non_empty_partitions_where_left_is_smaller(['a', 'a', 'c'])),
                         [
                             (['a'], ['a', 'c']),
                             (['a', 'a'], ['c'])
                         ])

    def test_all_expressions_with_only_addition(self):
        self.maxDiff = None
        x = [ "{} = {}".format(expr, expr.eval()) for expr in all_expressions([1,2,3,4], [ addition ]) ]
        self.assertEqual(x,
                         ['1 + 2 + 3 + 4 = 10',
                          '1 + 2 + 4 + 3 = 10',
                          '1 + 3 + 2 + 4 = 10',
                          '1 + 4 + 2 + 3 = 10',
                          '1 + 3 + 4 + 2 = 10',
                          '1 + 4 + 3 + 2 = 10'])
        
    def test_all_expressions_with_only_concat(self):
        self.maxDiff = None
        x = [ "{} = {}".format(expr, expr.eval()) for expr in all_expressions([1,2,3,4], [ concatenation ]) ]
        self.assertEqual(x,
                         ['1 concat 2 concat 3 concat 4 = 1234',
                          '1 concat 2 concat 4 concat 3 = 1243',
                          '1 concat 3 concat 2 concat 4 = 1324',
                          '1 concat 3 concat 4 concat 2 = 1342',
                          '1 concat 4 concat 2 concat 3 = 1423',
                          '1 concat 4 concat 3 concat 2 = 1432',
                          '2 concat 1 concat 3 concat 4 = 2134',
                          '2 concat 1 concat 4 concat 3 = 2143',
                          '2 concat 3 concat 1 concat 4 = 2314',
                          '2 concat 3 concat 4 concat 1 = 2341',
                          '2 concat 4 concat 1 concat 3 = 2413',
                          '2 concat 4 concat 3 concat 1 = 2431',
                          '3 concat 1 concat 2 concat 4 = 3124',
                          '3 concat 1 concat 4 concat 2 = 3142',
                          '3 concat 2 concat 1 concat 4 = 3214',
                          '3 concat 2 concat 4 concat 1 = 3241',
                          '3 concat 4 concat 1 concat 2 = 3412',
                          '3 concat 4 concat 2 concat 1 = 3421',
                          '4 concat 1 concat 2 concat 3 = 4123',
                          '4 concat 1 concat 3 concat 2 = 4132',
                          '4 concat 2 concat 1 concat 3 = 4213',
                          '4 concat 2 concat 3 concat 1 = 4231',
                          '4 concat 3 concat 1 concat 2 = 4312',
                          '4 concat 3 concat 2 concat 1 = 4321'])
        
def run_tests(args):
    sys.argv = [sys.argv[0]] + sys.argv[2:]
    unittest.main()

def example(args):
    addition = Operator(lambda a, b: a + b, '+')
    subtraction = Operator(lambda a, b: a - b, '-')
    print "a {} b".format(addition)
    a = Expression.number(5)
    b = Expression.number(6)
    expr = Expression.operation(addition, a, b)
    expr = Expression.operation(subtraction, expr, a)
    print "{} = {}".format(expr, expr.eval())

def parse_only_operators(args):
    return [ o for o in all_operators if o.str.strip() == args.only_operator ] if args.only_operator else all_operators

def all_expressions_command(args):
    numbers = [ int(x) for x in args.N ]
    operators = parse_only_operators(args)
    for expr in all_expressions(numbers, operators):
        x = expr.eval()
        if x:
            print "{} = {}".format(expr, x)

def all_valid_evaluated_expressions(numbers, operators):
    return [ x for x in [ expr.eval() for expr in all_expressions(numbers, operators) ] if x > 0 and float(x).is_integer() ]
        
def numbers_you_can_make_command(args):
    numbers = [ int(x) for x in args.N ]
    for x in sorted(list(set(all_valid_evaluated_expressions(numbers, all_operators)))):
        print x
        
def numbers_you_cant_make_command(args):
    numbers = [ int(x) for x in args.N ]
    up_to = args.up_to
    numbers_you_can_make = set(all_valid_evaluated_expressions(numbers, all_operators))
    for x in range(1, up_to + 1):
        if x not in numbers_you_can_make:
            print x

def how_can_you_make_command(args):
    target = args.target
    N = getattr(args, 'with')
    operators = parse_only_operators(args)
    numbers = [ int(x) for x in N ]
    for expr in [ expr for expr in all_expressions(numbers, operators) if expr.eval() == target ]:
        print "{} = {}".format(expr, expr.eval())
        
if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser(prog=sys.argv[0])
    subparsers = parser.add_subparsers()
    test_parser = subparsers.add_parser('test', help='run unit tests')
    test_parser.set_defaults(func=run_tests)

    example_parser = subparsers.add_parser('example', help='show example expression')
    example_parser.set_defaults(func=example)

    all_expressions_parser = subparsers.add_parser('all-expressions', help='show all expressions you can make')
    all_expressions_parser.add_argument('N')
    all_expressions_parser.add_argument('--only-operator')
    all_expressions_parser.set_defaults(func=all_expressions_command)

    numbers_you_can_make_parser = subparsers.add_parser('numbers-you-can-make', help='show numbers you can make')
    numbers_you_can_make_parser.add_argument('N')
    numbers_you_can_make_parser.set_defaults(func=numbers_you_can_make_command)

    numbers_you_cant_make_parser = subparsers.add_parser('numbers-you-cant-make', help='show numbers you cant make')
    numbers_you_cant_make_parser.add_argument('N')
    numbers_you_cant_make_parser.add_argument('--up-to', required = True, type = int)
    numbers_you_cant_make_parser.set_defaults(func=numbers_you_cant_make_command)

    how_can_you_make_parser = subparsers.add_parser('how-can-you-make', help='show how to make a given number')
    how_can_you_make_parser.add_argument('target', type = int)
    how_can_you_make_parser.add_argument('--with', required = True)
    how_can_you_make_parser.add_argument('--only-operator')
    how_can_you_make_parser.set_defaults(func=how_can_you_make_command)

    args = parser.parse_args()
    args.func(args)
    
