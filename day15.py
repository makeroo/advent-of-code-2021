import logging


logger = logging.getLogger(__name__)


class Path:
    ADJ = [(1, 0), (0, 1), (-1, 0), (0, -1)]

    def __init__(self, pt, parent=None, algo=None):
        if parent is None:
            self.cost = 0
        else:
            self.cost = parent.cost + algo.cell_at(*pt).me_cost

        self.head = pt
        self.parent = parent
        self._alive = True

    @property
    def alive(self):
        if not self._alive:
            return False

        p = self.parent

        while p is not None:
            if not p._alive:
                return False

            p = p.parent

        return True

    def derive(self, algo):
        x, y = self.head

        for dx, dy in self.ADJ:
            nx = x + dx
            ny = y + dy

            if nx < 0 or ny < 0 or nx >= algo.width or ny >= algo.height:
                continue

            yield Path((nx, ny), self, algo)

    def dead(self):
        self._alive = False

        logger.debug('dead end: %s', self)

    def __str__(self):
        return f'{"dead:" if not self.alive else ""}{self.cost}:({self.head[0]},{self.head[1]}){self.parent or ""}'


class Cell:
    def __init__(self, c):
        self.min_path = None
        self.min_cost = None
        self.me_cost = c

    def owned_by(self, p):
        if self.min_path is not None:
            self.min_path.dead()

        self.min_path = p
        self.min_cost = p.cost

    def add(self, f):
        c = self.me_cost + f

        if c > 9:
            c -= 9

        return Cell(c)

    def __str__(self):
        return f'{self.me_cost}:{self.min_cost}:{self.min_path}'

    __repr__ = __str__


class Algo:
    def __init__(self, m):
        self.m = m
        self.height = len(m)
        self.width = len(m[0])

    def solve(self):
        self.paths = [Path((0, 0))]

        while self.paths:
            self._step()

        c = self.cell_at(self.width - 1, self.height - 1)

        return c.min_path and c.min_path.cost

    def cell_at(self, x: int, y: int) -> Cell:
        return self.m[y][x]

    def _step(self):
        new_paths = []

        for p in self.paths:
            if not p.alive:
                continue

            logger.debug('deriving path: %s', p)

            c = self.cell_at(*p.head)

            if c.min_cost is not None and p.cost >= c.min_cost:
                p.dead()
                continue

            c.owned_by(p)

            new_paths.extend(p.derive(self))

        self.paths = new_paths


def enlarge_map(m: list[list[Cell]]) -> list[list[Cell]]:
    r = []

    for ym in range(5):
        for row in m:
            new_row = []

            r.append(new_row)

            for xm in range(5):
                for c in row:
                    new_row.append(c.add(xm + ym))

    return r


def parse15(p):
    z = ord('0')

    with open(p) as f:
        r = f.readlines()

    return [[Cell(ord(x) - z) for x in line.strip()] for line in r]


if __name__ == '__main__':
    # logging.basicConfig(level=logging.DEBUG)
    m = parse15('day15.input')
    r = Algo(m).solve()
    print(r)
    mxl = enlarge_map(m)
    # print('length: ', len(mxl), "height:", len(mxl[0]))
    for row in mxl:
        # for cell in row:
        #    print(cell.me_cost, end='')
        print()
    r = Algo(mxl).solve()
    print(r)
