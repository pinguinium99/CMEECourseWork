#!/usr/bin/env python3

""" function for aligning sequences and providing the best score and producing an ouput file """

__author__ = 'Prannoy T (p.thazhakaden23@.imperial.ac.uk)'
__version__ = '0.0.1'

## imports ##
import sys # module to interface our program with the operating system
import csv
## constants ##

## functions ##

def split_seq():
    """ Finds and opens the input file and splits the sequences and counts the length """
    bothseq = '../Data/align_seq.csv'
    with open(bothseq, "r" ) as seq:
        seq1 = seq.readline().strip()
        seq2 = seq.readline().strip()
        l1 = len(seq1)
        l2 = len(seq2)
        if l1 >= l2:
            s1 = seq1
            s2 = seq2
        else:
            s1 = seq2
            s2 = seq1
        l1, l2 = l2, l1                   # swap the two lengths
    return s1, s2, l1, l2

# A function that computes a score by returning the number of matches starting
# from arbitrary startpoint (chosen by user)

def calculate_score(s1, s2, l1, l2, startpoint):
    """ Calculates the best alignment between the two sequences"""
    matched = "" # to hold string displaying alignements
    score = 0
    for i in range(l2):
        if (i + startpoint) < l1:
            if s1[i + startpoint] == s2[i]: # if the bases match
                matched = matched + "*"
                score = score + 1
            else:
                matched = matched + "-"

    # some formatted output
    print("." * startpoint + matched)           
    print("." * startpoint + s2)
    print(s1)
    print(score) 
    print(" ")

    return score

# Test the function with some example starting points:
# calculate_score(s1, s2, l1, l2, 0)
# calculate_score(s1, s2, l1, l2, 1)
# calculate_score(s1, s2, l1, l2, 5)

# now try to find the best match (highest score) for the two sequences


def best_score(s1, s2, l1, l2):
    """ Finds best alignment and ouputs the sequences and score """
    my_best_align = None
    my_best_score = -1
    for i in range(l1): # Note that you just take the last alignment with the highest score
        z = calculate_score(s1, s2, l1, l2, i)
        if z > my_best_score:
            my_best_align = "." * i + s2 # think about what this is doing!
            my_best_score = z 
    t = open('../Results/Align_seq.txt','x')
    print(my_best_align)
    print(s1)
    print("Best score:", my_best_score)
    t.write(str(my_best_align) + "\n" + str(s1) +"\n" + "Best score:" + str(my_best_score))  


def main(argv):
    split_seq()
    test = list(split_seq())
    test.append(1)
    calculate_score(*test)
    best_score(*(split_seq()))
    return 0

if __name__ == "__main__":
    """Makes sure the "main" function is called from the command line"""
    status = main(sys.argv) 


# need to add if statement for taking input file names