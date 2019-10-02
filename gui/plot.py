import numpy as np
#if you utilize the following two lines you will be able to run 
#the figures in here. This requires changing the backend of the fig.show()
#for more backend choices please see https://matplotlib.org/tutorials/introductory/usage.html#what-is-a-backend
import matplotlib
#Qt5Agg is the backend
matplotlib.use('Qt5Agg')
import matplotlib.pyplot as plt
import argparse

def shift(X):
    # This is the mapping from CrystalGen coords to -211
    return (X + 195)%360

parser = argparse.ArgumentParser()
parser.add_argument('-f', '--file', help='File to plot')
parser.add_argument('-n', '--normalize', action='store_true', help='Should the values be normalized')
parser.add_argument('-s', '--shift', action='store_true', help='Should the graph be shifted by shift function')
args = parser.parse_args()

data = open(args.file, 'r')

D = []

for line in data:
    val = line.split()
    D.append([float(val[0]),float(val[1])])

D = np.array(D)
if args.normalize:
    yMin = np.min(D[:,1])
    yMax = np.max(D[:,1])
    diff = yMax - yMin
    D[:,1] = (D[:,1] - yMin)/diff
if args.shift:
    print('Shifting')
    D[:,0] = shift(D[:,0])

#Sorts by the X column, This makes the line plot nice
D = D[D[:,0].argsort()]

fig, ax = plt.subplots()
ax.plot(D[:,0], D[:,1])
ax.scatter(D[:,0], D[:,1])
fig.show()
input("Enter to Exit")
