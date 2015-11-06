# mine_solver
Solver for mines / minsweeper shapes

Given a specification for overlapping sets of numbered tiles that must
contain a certain number of mines calculate all possible solutions
containing the numbered cells containing mines.

An example spec would be:

    [{[1,2,3,15,16],2},
     {[2,3,4],1},
     {[3,4,5,6,7],2},
     {[6,7,8],1},
     {[7,8,9,10,11],2},
     {[10,11,12],1},
     {[11,12,13,14,15],1},
     {[14,15,16],1}]

Cells 1-3, 15 and 16 must have 2 mines. That is, some cell connected to
all of those cells has reported that it is adjacent to two mines.

The example spec would be for a section of a "mines" / "minesweeper"
like the following:

    1   2  3  4  5  The numbers on the outside are the cells that mines
    16  2  1  2  6  can be in and the numbers on the inside are the
    15  1     1  7  reported numbers of adjacent mines.
    14  1  1  2  8
    13  12 11 10 9

Once we have the solutions we can calculate the probability that a mine
will be in a particular cell. Each cell can only have one mine so it
will only appear once in a solution. If we take the number of times that
cell appears (i.e. it has a mine) divided by the number of solutions,
we'll have the probability that the actual solution (i.e. in the current
game whatever pattern the mines are laid out it) will be one that has a
mine in that cell.

    41%  45%  31%  24%  60%
    59%  2    1    2    52%
    24%  1         1    33%
    17%  1    1    2    16%
    24%  10%  24%  65%  62%
