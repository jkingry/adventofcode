import itertools

inputF = open('../input/2.txt', 'r')
input = inputF.readlines()

def part1():
    parse = map(int, input)
    for (x,y) in itertools.combinations(parse, 2):
        if x + y == 2020:
            return x * y

print("part1: %d" % part1())

def part2():
    parse = map(int, input)
    for (x, y, z) in itertools.combinations(parse, 3):
        if x + y + z == 2020:
            return x * y * z


print("part2: %d" % part2())
