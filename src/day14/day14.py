from functools import cache


def part1(inp):
    grid = inp.splitlines()
    nr, nc = len(grid), len(grid[0])
    res = 0
    for c in range(nc):
        last_barrier = nr
        for r in range(nr):
            if grid[r][c] == ".":
                continue
            if grid[r][c] == "#":
                last_barrier = nr - r - 1
            if grid[r][c] == "O":
                res += last_barrier
                last_barrier -= 1
    return res


def N(grid):
    nr, nc = len(grid), len(grid[0])
    new_grid = [["."] * nc for _ in range(nr)]
    for c in range(nc):
        last_barrier = 0
        for r in range(nr):
            if grid[r][c] == ".":
                continue
            if grid[r][c] == "#":
                last_barrier = r + 1
                new_grid[r][c] = "#"
            if grid[r][c] == "O":
                new_grid[last_barrier][c] = "O"
                last_barrier += 1
    return tuple(map(tuple, new_grid))


def W(grid):
    nr, nc = len(grid), len(grid[0])
    new_grid = [["."] * nc for _ in range(nr)]
    for r in range(nr):
        last_barrier = 0
        for c in range(nc):
            if grid[r][c] == ".":
                continue
            if grid[r][c] == "#":
                last_barrier = c + 1
                new_grid[r][c] = "#"
            if grid[r][c] == "O":
                new_grid[r][last_barrier] = "O"
                last_barrier += 1
    return tuple(map(tuple, new_grid))


def S(grid):
    return N(grid[::-1])[::-1]


def E(grid):
    return tuple(row[::-1] for row in W([row[::-1] for row in grid]))


@cache
def spin(grid):
    return E(S(W(N(grid))))


def load(grid):
    res = 0
    for r, row in enumerate(grid):
        res += row.count("O") * (len(grid) - r)
    return res


def part2(inp):
    grid = tuple(map(tuple, inp.splitlines()))
    cache = {}
    rev_cache = {}
    for i in range(1_000_000_000):
        cache[grid] = i
        rev_cache[i] = grid
        grid = spin(grid)
        if grid in cache:
            first_rep = cache[grid]
            cyc_len = i + 1 - cache[grid]
            break
    return load(rev_cache[first_rep + (1_000_000_000 - first_rep) % cyc_len])


def main():
    with open("input.txt") as f:
        inp = f.read()
    print(part1(inp))
    print(part2(inp))


main()
