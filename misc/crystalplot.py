# This import registers the 3D projection, but is otherwise unused.
from mpl_toolkits.mplot3d import Axes3D  # noqa: F401 unused import

import matplotlib.pyplot as plt
import numpy as np
import os
import math

fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')

file = open('crystal.input', 'r')

for line in file:
    arr = line.split()
    xs = float(arr[0].replace('D0','').replace('d0',''))
    ys = float(arr[1].replace('D0','').replace('d0',''))
    zs = float(arr[2].replace('D0','').replace('d0',''))
    if math.fabs(xs) <= 15 and math.fabs(ys) < 15:
        ax.scatter(xs, ys, zs, c='r')

file.close()

ax.set_xlim(-15, 15)
ax.set_ylim(-15, 15)
ax.set_zlim(-5, 5)

ax.set_xlabel('X Label')
ax.set_ylabel('Y Label')
ax.set_zlabel('Z Label')

plt.show()