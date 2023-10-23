#!/usr/bin/env python3

"""Some functions exemplifying the use of control statements"""
#docstrings are considered part of the running code (normal comments are
#stripped). Hence, you can access your docstrings at run time.

__author__ = 'Prannoy T (p.thazhakaden23@.imperial.ac.uk)'
__version__ = '0.0.1'

## imports ##
import sys # module to interface our program with the operating system
import doctest #

## constants ##

## functions ##
def even_or_odd(x=0): # if not specified, x should take value 0.
    """Find whether a number x is even or odd.
      
    >>> even_or_odd(10)
    '10 is Even!'
    
    >>> even_or_odd(7)
    '7 is Odd!'
        
    in case of negative numbers, the positive is taken:    
    >>> even_or_odd(-2)
    '-2 is Even!'
    
    """
    #Define function to be tested

    
    """Find whether a number x is even or odd."""
    if x % 2 == 0: #The conditional if
        return f"{x} is Even!"
    return f"{x} is Odd!"

def main(argv):
    print(even_or_odd(22))
    print(even_or_odd(33))
    return 0


if __name__ == "__main__":
    """Makes sure the "main" function is called from the command line"""
    status = main(sys.argv)
    doctest.testmod()
