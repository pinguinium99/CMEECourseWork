#!/usr/bin/env python3

""" A python script to populate a dictionary called taxa_dic derived from taxa  """

__author__ = 'Prannoy T (p.thazhakaden23@.imperial.ac.uk)'
__version__ = '0.0.1'

## imports ##
import sys # module to interface our program with the operating system

## constants ##

## functions ##


taxa = [ ('Myotis lucifugus','Chiroptera'),
         ('Gerbillus henleyi','Rodentia',),
         ('Peromyscus crinitus', 'Rodentia'),
         ('Mus domesticus', 'Rodentia'),
         ('Cleithrionomys rutilus', 'Rodentia'),
         ('Microgale dobsoni', 'Afrosoricida'),
         ('Microgale talazaci', 'Afrosoricida'),
         ('Lyacon pictus', 'Carnivora'),
         ('Arctocephalus gazella', 'Carnivora'),
         ('Canis lupus', 'Carnivora'),
        ]

# Write a python script to populate a dictionary called taxa_dic derived from
# taxa so that it maps order names to sets of taxa and prints it to screen.
# 
# An example output is:
#  
# 'Chiroptera' : set(['Myotis lucifugus']) ... etc. 
# OR, 
# 'Chiroptera': {'Myotis  lucifugus'} ... etc

taxa_dic = {}

def taxat2d(taxatup, taxadict):
        """ A function to create dictionary from taxa """
        for i, j in taxa:
                taxadict.setdefault(j,[]).append(i)
        return taxa_dic

print(taxat2d(taxa,taxa_dic))

# Now write a list comprehension that does the same (including the printing after the dictionary has been created)  

taxa_dic = {x[1]: ([y[0] for y in taxa if y[1] == x[1]]) for x in taxa}

print(taxa_dic)

 
