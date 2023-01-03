import numpy as np
from scipy.optimize import fmin_cobyla
from scipy.optimize import minimize_scalar
import matplotlib.pyplot as plt
from math import hypot
import sys
from collections import defaultdict as d
from optparse import OptionParser, OptionGroup

# Author: Martin Kapun

#########################################################   HELP   #########################################################################
usage = "python %prog --input file --output file "
parser = OptionParser(usage=usage)
group = OptionGroup(parser, '< put description here >')

#########################################################   CODE   #########################################################################

parser.add_option("--input", dest="IN", help="Input file")
parser.add_option("--low", dest="low", help="")
parser.add_option("--mid", dest="mid", help="")
parser.add_option("--high", dest="high", help="")
parser.add_option("--output", dest="OUT", help="Output file", default="NA")

(options, args) = parser.parse_args()
parser.add_option_group(group)


def makeVandermondeMatrix(a, b, c):
    ''' make VandermondeMatrix : https://www.youtube.com/watch?v=vEvbNG-kRyY'''
    ax, ay = a
    bx, by = b
    cx, cy = c
    print(a, b, c)
    return np.array([[1, ax, ax**2], [1, bx, bx**2], [1, cx, cx**2]])
    # return np.array([[1,1,1],[ax,bx,cx],[ax**2,bx**2,cx**2]])


def solve4x(Y):
    ''' solve y=ax**2+bx+c for x: https://www.quora.com/How-do-you-solve-for-x-in-a-quadratic-equation-ax-2-bx-c '''
    x1 = ((B**2-4*A*(C-Y))**0.5-B)/2*A
    x2 = (-(B**2-4*A*(C-Y))**0.5-B)/2*A
    return x1, x2


def f(x):
    ''' https://kitchingroup.cheme.cmu.edu/blog/2013/02/14/Find-the-minimum-distance-from-a-point-to-a-curve/ '''
    return A*x**2+B*x+C


def obj2(x):
    y = f(x)
    return np.sqrt((x - P[0])**2 + (y - P[1])**2)


def arclength(f, a, b, tol=1e-6):
    """Compute the arc length of function f(x) for a <= x <= b. Stop
    when two consecutive approximations are closer than the value of
    tol.
    https://stackoverflow.com/questions/46098157/how-to-calculate-the-length-of-a-curve-of-a-math-function
    """
    nsteps = 1  # number of steps to compute
    oldlength = 1.0e20
    length = 1.0e10
    while abs(oldlength - length) >= tol:
        nsteps *= 2
        fx1 = f(a)
        xdel = (b - a) / nsteps  # space between x-values
        oldlength = length
        length = 0
        for i in range(1, nsteps + 1):
            fx0 = fx1  # previous function value
            fx1 = f(a + i * (b - a) / nsteps)  # new function value
            length += hypot(xdel, fx1 - fx0)  # length of small line segment
    return length


# get X,Y and temperature values
Low = [float(x) for x in options.low.split(",")]
Mid = [float(x) for x in options.mid.split(",")]
High = [float(x) for x in options.high.split(",")]
###
V = makeVandermondeMatrix(Low[:2], Mid[:2], High[:2])
# print(V)
vy = np.array([Low[1], Mid[1], High[1]])
va = np.linalg.inv(V).dot(vy)
C, B, A = va
# print(vy,va)
OUT = open(options.OUT+".txt", "wt")

# make vector with 100 dots between Low and High x
x = np.linspace(Low[0], High[0], 100)
FL = arclength(f, Low[0], High[0], 1e-10)
fig = plt.figure()
ax = fig.add_subplot(111)

plt.plot(x, f(x), label='f(x)', color="black")
plt.gca().set_xlim(0, 12)
plt.plot(Low[0], Low[1], marker='o', color="black")
plt.plot(Mid[0], Mid[1], marker='o', color="black")
plt.plot(High[0], High[1], marker='o', color="black")
NO = int(High[2])-int(Low[2])
colors = plt.cm.jet(np.linspace(0, 1, NO+1))

# print header
FILE = open(options.IN, "rt")
header = FILE.readline()
OUT.write(header.rstrip()+"\tTempEst\n")

# loop through coordinates in file
for l in FILE:
    a = l.rstrip().split()
    P = [float(x) for x in a[:2]]

    # calculate minimmum distance bound to Low/High https://docs.scipy.org/doc/scipy/reference/generated/scipy.optimize.minimize_scalar.html
    if Low[0] < High[0]:
        X2 = minimize_scalar(obj2, bounds=(Low[0], High[0]), method='bounded')
        LE = arclength(f, Low[0], X2.x, 1e-10)
        Temp = Low[2]+LE*(High[2]-Low[2])/FL
    else:
        X2 = minimize_scalar(obj2, bounds=(High[0], Low[0]), method='bounded')
        LE = arclength(f, High[0], X2.x, 1e-10)
        Temp = High[2]-LE*(High[2]-Low[2])/FL
    OUT.write(l.rstrip()+"\t"+str(round(Temp, 3))+"\n")
    plt.plot(P[0], P[1], marker='o', color=colors[(
        int(High[2])-int(Low[2]))-(int(High[2])-int(Temp))])
    plt.plot(X2.x, f(X2.x), marker='o', color=colors[(
        int(High[2])-int(Low[2]))-(int(High[2])-int(Temp))])
    plt.plot([P[0], X2.x], [P[1], f(X2.x)], color=colors[(
        int(High[2])-int(Low[2]))-(int(High[2])-int(Temp))])

plt.xlabel('x')
plt.axis("equal")

plt.ylabel('y')
plt.legend(loc='best')
plt.gca().invert_yaxis()


# plt.show()
if options.OUT != "NA":
    plt.savefig(options.OUT+".pdf", format="pdf")
