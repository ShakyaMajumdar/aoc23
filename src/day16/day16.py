def energy(grid, start):
    nr, nc = len(grid), len(grid[0])
    energized = [[False] * nc for _ in range(nr)]
    m1_map = {1: -1j, -1: 1j, 1j: -1, -1j: 1}
    m2_map = {1: 1j, -1: -1j, 1j: 1, -1j: -1}

    visited = set()
    q = [start]
    while q:
        pos, dir = q.pop()
        if (pos, dir) in visited:
            continue
        visited.add((pos, dir))
        if 0 <= pos.real < nc and 0 <= pos.imag < nr:
            energized[int(pos.imag)][int(pos.real)] = True
        nxt_pos = pos + dir
        if not (0 <= nxt_pos.imag < nr and 0 <= nxt_pos.real < nc):
            continue
        nxt_val = grid[int(nxt_pos.imag)][int(nxt_pos.real)]
        if nxt_val == ".":
            q.append((nxt_pos, dir))
        if nxt_val == "/":
            q.append((nxt_pos, m1_map[dir]))
        if nxt_val == "\\":
            q.append((nxt_pos, m2_map[dir]))
        if nxt_val == "|":
            if dir in (1, -1):
                q.append((nxt_pos, 1j))
                q.append((nxt_pos, -1j))
            else:
                q.append((nxt_pos, dir))
        if nxt_val == "-":
            if dir in (1j, -1j):
                q.append((nxt_pos, 1))
                q.append((nxt_pos, -1))
            else:
                q.append((nxt_pos, dir))
    return sum(map(sum, energized))


def part1(inp):
    grid = tuple(map(tuple, inp.splitlines()))
    return energy(grid, (-1, 1))


def part2(inp):
    grid = tuple(map(tuple, inp.splitlines()))
    nr, nc = len(grid), len(grid[0])
    candidates = (
        [(-1 + r * 1j, 1) for r in range(nr)]
        + [(nc + r * 1j, -1) for r in range(nr)]
        + [(c - 1j, 1j) for c in range(nc)]
        + [(c + nr * 1j, -1j) for c in range(nc)]
    )
    return max(energy(grid, candidate) for candidate in candidates)


def main():
    with open("input.txt") as f:
        inp = f.read()
    print(part1(inp))
    print(part2(inp))


main()
