import itertools
import copy

inputF = open('../input/17.txt', 'r')
input = inputF.readlines()

class PocketPoint:
    def __init__(self, pt, v):
        self.pt = pt
        self.v = v

class  PocketState:
    def __init__(self, p = None) -> None:
        self.tl = p.tl if p else (0, 0, 0)
        self.br = p.br if p else (0, 0, 0)
        self.data = copy.deepcopy(p.data) if p else {}

    def isactive(self, pt):
        (x, y, z) = pt
        return self.data.get(z, False) \
            and self.data[z].get(x, False) \
            and self.data[z][x].get(y, False) == "#"

    def set(self, pt, v):
        (x, y, z) = pt
        if v == "#":
            self.tl = tuple(map(min, zip(pt, self.tl)))
            self.br = tuple(map(max, zip(pt, self.br)))

        self.data.setdefault(z, {}).setdefault(x, {})[y] = v

    def adj(self, pt):
        (x, y, z) = pt
        for (dx, dy, dz) in itertools.product(range(-1, 2), range(-1, 2), range(-1, 2)):
            if dx == 0 and dy == 0 and dz == 0: 
                continue
            yield self.isactive((x + dx, y + dy, z + dz))

    def getpoints(self):        
        for z in range(self.tl[2] - 1, self.br[2] + 1):
            for x in range(self.tl[0] - 1, self.br[0] + 1):
                for y in range(self.tl[1] - 1, self.br[1] + 1):
                    yield PocketPoint((x, y, z), "#" if self.isactive((x, y, z)) else ".")

    def print(self):
        for z in self.data:
            print("z=%d" % z)
            for x in self.data[z]:
                for y in self.data[z][x]:
                    print(self.data[z][x][y], end='')
                print()
            print()


def parse(lines):
    p = PocketState()
    for x, line in enumerate(input):
        for y, state in enumerate(line.strip()):
            p.set((x, y, 0), state)
    return p

def part1():
    p = parse(input)
    p.print()

    np = PocketState(p)

    for t in p.getpoints():
        active = sum(1 for _ in filter(lambda x: x, p.adj(t.pt)))
        
        if t.v == "#" and active != 2 and active != 3:
            np.set(t.pt, ".")
        elif t.v == "." and active == 3:
            np.set(t.pt, "#")     
    
    np.print()    

    return -1

print("part1: %d" % part1())

def part2():
    return -1

print("part2: %d" % part2())
