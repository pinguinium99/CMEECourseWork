#!/usr/bin/env python3

"""Some functions exemplifying docstrings in Practical"""

__author__ = 'Prannoy T (p.thazhakaden23@.imperial.ac.uk)'
__version__ = '0.0.1'

## imports ##
import sys 

## constants ##

## functions ##

def foo_1(x = 9):
    """Find square root of x."""
    return f"The square root of {x} is {x ** 0.5}" 


def foo_2(x = 5, y = 8):
    """ Finding the greater number in x and y. """
    if x > y:
        return f"{x} is greater"
    return f"{y} is greater"


def foo_3(x = 9, y = 5, z = 8):
    """ Ordering x y and z in order of size. """
    if x > y:
        tmp = y
        y = x
        x = tmp
    if y > z:
        tmp = z
        z = y
        y = tmp
    return f"{x, y, z} is the correct order"
 

def foo_4(x = 6):
    """ Return the factorial of x """
    result = 1
    for i in range(1, x + 1):
        result = result * i
    return f" The factorial of {x} is {result} "


def foo_5(x = 6):
    """ Return the factorial of x """
    if x == 1:
        return 1
    if x == 0:
        return 1
    return x * foo_5(x - 1) # how to print output text?


def foo_6(x = 6):
    """ Return the factorial of x """
    xf = x
    facto = 1
    while x >= 1:
        facto = facto * x
        x = x - 1
    return f"The factorial of {xf} is {facto}"


def main(argv):
    print(foo_1(9))
    print(foo_2(5,8))
    print(foo_3(9,5,8))
    print(foo_4(5))
    print(foo_5(6))
    print(foo_6(6))
    return 0


if __name__ == "__main__":
    """Makes sure the "main" function is called from the command line"""
    status = main(sys.argv)
    sys.exit(status)

