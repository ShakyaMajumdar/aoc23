def hor_mirror(grid):
    m, n = len(grid), len(grid[0])
    for r in range(1, m):
        sec1 = grid[r - 1 :: -1]
        sec2 = grid[r:]
        valid = min(len(sec1), len(sec2))
        if sec1[:valid] == sec2[:valid]:
            return r


def ver_mirror(grid):
    return hor_mirror([*zip(*grid)])


def parse(s):
    return [i.splitlines() for i in s.split("\n\n")]


def part1(inp):
    grids = parse(inp)
    res = 0
    for grid in grids:
        if (h := hor_mirror(grid)) is not None:
            res += 100 * h
        elif (v := ver_mirror(grid)) is not None:
            res += v
    return res


def grid2bin(grid):
    rows = [int(row.replace("#", "1").replace(".", "0"), 2) for row in grid]
    cols = [
        int("".join(col).replace("#", "1").replace(".", "0"), 2) for col in zip(*grid)
    ]
    return rows, cols


def join_bins(xs, n):
    res = 0
    for x in xs:
        res <<= n
        res |= x
    return res


def hor_mirror2(grid):
    rows, cols = grid2bin(grid)
    m, n = len(rows), len(cols)
    for r in range(1, m):
        sec1 = rows[r - 1 :: -1]
        sec2 = rows[r:]
        valid = min(len(sec1), len(sec2))
        if (join_bins(sec1[:valid], n) ^ join_bins(sec2[:valid], n)).bit_count() == 1:
            return r


def ver_mirror2(grid):
    return hor_mirror2(["".join(c) for c in zip(*grid)])


def part2(inp):
    grids = parse(inp)
    res = 0
    for grid in grids:
        if (h := hor_mirror2(grid)) is not None:
            res += 100 * h
        elif (v := ver_mirror2(grid)) is not None:
            res += v
    return res


def main():
    with open("input.txt") as f:
        inp = f.read()
    print(part1(inp))
    print(part2(inp))


main()
