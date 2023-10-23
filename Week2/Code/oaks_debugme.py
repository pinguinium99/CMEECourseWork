#!/usr/bin/env python3

""" Function for finding oaks from a data set and outputing it in another file """

__author__ = 'Prannoy T (p.thazhakaden23@.imperial.ac.uk)'
__version__ = '0.0.1'

## imports ##

import csv
import sys
import doctest

## functions ##


def is_an_oak(name):
    """ Returns True if name is starts with 'quercus' 
    >>> is_an_oak('Quercus petraea')
    True

    >>> is_an_oak('Pinus sylvestris')
    False

    >>> is_an_oak('Quercuss petraea')
    False

    """

    return name.lower().startswith('quercus ') #space makes sure that quercus is not unintended


def main(argv): 
    """ Opens data file and writes in an output file with just oak data """

    f = open('../Data/TestOaksData.csv','r')
    g = open('../Data/JustOaksData.csv','w')
    taxa = csv.reader(f)
    csvwrite = csv.writer(g)
    oaks = set()
    for row in taxa:
        print(row)
        print ("The genus is: ") 
        print(row[0] + '\n')
        if is_an_oak(row[0]):
            print('FOUND AN OAK!\n')
            csvwrite.writerow([row[0], row[1]])    

    return 0
if (__name__ == "__main__"):
    status = main(sys.argv)
    doctest.testmod()