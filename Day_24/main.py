#!/usr/bin/env python
import copy
import sys

from enum import Enum

DIRS = [(0, -1), (0, 1), (-1, 0), (1, 0)]
class DirEnum(Enum):
    UP = 0
    DOWN = 1
    LEFT = 2
    RIGHT = 3

    def get(self):
        return DIRS[self.value]

    def rev(self):
        idx = (self.value // 2 + 1) % 2 + (self.value // 2) * 2
        return DirEnum(idx)

    def step(self):
        if self == DirEnum.UP or self == DirEnum.DOWN:
            return (0, 5)
        else:
            return (5, 0)


def apply_dir(loc, dir):
    return tuple(sum(x) for x in zip(loc, dir))

class Point:
    def __init__(self, value, loc):
        self.loc = loc
        self.value = value

    def step(self, grid, smaller, bigger):
        if self.loc == (2, 2):
            return
        count = sum(grid.get(self.loc, dir, smaller, bigger) for dir in DirEnum)
        if self.value == 1:
            self.value = 1 if count == 1 else 0
        else:
            self.value = 1 if count == 1 or count == 2 else 0

    def __str__(self):
        if (self.loc == (2, 2)):
            return '?'
        if self.value == 1:
            return '#'
        else:
            return '.'

class Grid:
    @staticmethod
    def empty():
        return Grid([[Point(0, (i, j)) for i in range(5)] for j in range(5)])

    @staticmethod
    def from_stdin():
        stdin = [[Point(0, (i, j)) if x == '.' else Point(1, (i, j)) for i, x in enumerate(row[:-1])] for j, row in enumerate(sys.stdin.readlines())]
        return Grid(stdin)

    def __init__(self, data):
        self.grid = data

    def get_for_bigger(self, dir):
        if dir == DirEnum.UP:
            return sum(x.value for x in  self.grid[-1])
        if dir == DirEnum.DOWN:
            return sum(x.value for x in self.grid[0])
        if dir == DirEnum.RIGHT:
            return sum(x[0].value for x in self.grid)
        return sum(x[-1].value for x in self.grid)

    def get(self, init, dir, smaller, bigger):
        loc = apply_dir(init, dir.get())
        if (max(loc) > 4 or min(loc) < 0):
            if dir == DirEnum.UP:
                return bigger.grid[1][2].value
            if dir == DirEnum.DOWN:
                return bigger.grid[3][2].value

            if dir == DirEnum.LEFT:
                return bigger.grid[2][1].value
            if dir == DirEnum.RIGHT:
                return bigger.grid[2][3].value

        if (loc == (2, 2)):
            return smaller.get_for_bigger(dir)

        (i, j) = loc
        return self.grid[j][i].value

    def step(self, smaller, bigger):
        new_self = copy.deepcopy(self)
        for row in new_self.grid:
            for x in row:
                x.step(self, smaller, bigger)
        return new_self

    def count(self):
        return sum(sum(x.value for x in  row) for row in self.grid)

    def __str__(self):
        out = "\n"
        return  "\n".join("".join(str(x) for x in row) for row in self.grid) + out

    def __repr__(self):
        out = "Grid<\n"
        return out + "\n".join("".join(str(x) for x in row) for row in self.grid) + ">\n"


def main():
    state = [Grid.from_stdin()]

    mins = 200

    for i in range(mins):
        if (i % 2 == 0):
            state = [Grid.empty()] + state + [Grid.empty()]
        state = [x.step(b, s) for s, x, b in zip(
            [Grid.empty()] + state[:-1],
            state,
            state[1:] + [Grid.empty()]
        )]

    print(sum(x.count() for x in state))


if __name__ == '__main__':
    main()
