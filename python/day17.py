from itertools import count
import math
from time import sleep
from typing import Optional


Range = tuple[float, float]
Velocity = tuple[float, float]


class Point:
    def __init__(self, x: float = 0, y: float = 0) -> None:
        self.x = x
        self.y = y

    def __add__(self, t: Velocity) -> 'Point':
        return Point(self.x + t[0], self.y + t[1])

    def in_range(self, xrange: Range, yrange: Range) -> bool:
        return xrange[0] <= self.x <= xrange[1] and yrange[0] <= self.y <= yrange[1]

    def __str__(self) -> str:
        return f'({self.x},{self.y})'


def maybe_valid(p: Point, xrange: Range, yrange: Range) -> bool:
    return p.x <= max(xrange) and p.y >= min(yrange)


# def trace(x, y):
#    while True:
#        yield x, y
#
#        if x > 0:
#            x -= 1
#
#        y -= 1


def check_trace(vel: Velocity, xrange: Range, yrange: Range) -> Optional[float]:
    p = Point()

    max_y = p.y
    # i = 0

    while maybe_valid(p, xrange, yrange):
        # print('iter:', i, ' max y:', max_y, ' point:', p)

        if p.in_range(xrange, yrange):
            # print('found')
            return max_y

        p = p + vel

        if p.y > max_y:
            max_y = p.y

        vel = (max(0, vel[0] - 1), vel[1] - 1)

        # i += 1

    return None


def min_x_vel(xrange: Range) -> int:
    min_x = min(xrange)

    n = (-1 + math.sqrt(1 + 8 * min_x)) / 2

    return int(math.ceil(n))


def how_many_steps(vx: int, xrange: Range) -> Optional[int]:
    x = 0
    i = 0

    while True:
        if x > xrange[1]:
            return None
        elif x >= xrange[0]:
            return i
        elif vx == 0:
            return None

        x += vx
        vx = max(0, vx - 1)
        i += 1


def max_y_vel(xrange: Range, yrange: Range, vx: Optional[int] = None):
    min_vx = vx or min_x_vel(xrange)
    max_y = None
    found = 0

    for y in count():
        my = check_trace([min_vx, y], xrange, yrange)

        if my is None:
            continue

        found += 1

        if max_y is None or my > max_y:
            max_y = my

        print(f'y={y}, hit: {my}, max: {max_y}, found: {found}')

        # sleep(0.5)


def matching_velocities(xrange: Range, yrange: Range) -> int:
    found = 0

    min_vx = min_x_vel(xrange)
    max_vx = max(xrange) + 1

    max_h = max(yrange)
    min_h = min(yrange)

    for vx in range(min_vx, max_vx + 1):
        n = how_many_steps(vx, xrange)

        if n is None:
            continue

        max_y = int((max_h + n * (n - 1) / 2) / n) + 1 + 1000
        min_y = min_h

        print(vx, max_y)

        for vy in range(min_y, max_y + 1):
            if check_trace([vx, vy], xrange, yrange) is not None:
                found += 1

    return found

    # max_y = min(yrange)
    # return max_y / min_vx - 1 + (min_vx + 1) / 2
    # min_y_vel =


# def check_y_formula(vy):
#    p = 0
#    i = 1
#    while i < 100:
#        p += vy - i + 1

# print(i, p, i * (vy + 1) - i * (i + 1) / 2)

#       assert p == i * (vy + 1) - i * (i + 1) / 2

#       i += 1


# xrange = (20, 30)
# yrange = (-10, -5)
#
# p = [0, 0]

matching_velocities([217, 240], [-126, -69])
