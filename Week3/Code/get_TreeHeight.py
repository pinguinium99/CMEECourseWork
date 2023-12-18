
#!/usr/bin/env python3

""" Function for calculating tree heights from angle and distance """

#__author__ = 'Prannoy T (p.thazhakaden23@.imperial.ac.uk)'
#__version__ = '0.0.1'

## imports ##

import sys
import pandas
import os
import numpy as np

data = sys.argv[1]
Treedat = pandas.read_csv(data)

Treedat = (pandas.read_csv('../Data/trees.csv'))

def Treeheight(degrees,distance):
    """ calculates the height of a tree using angle and distance """
    height = distance * np.tan(np.deg2rad(degrees))
    return height

Treeheight(30,40)

height = Treeheight(Treedat['Angle.degrees'], Treedat['Distance.m'])

Treedat.insert(3,"Heights",height)

output_name = os.path.splitext(os.path.basename(sys.argv[1]))[0]
output_path = os.path.join("../Results", f"{output_name}_treeheights.csv")
Treedat.to_csv(output_path, index = False)
