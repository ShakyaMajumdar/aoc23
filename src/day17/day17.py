from heapq import heappop, heappush
from itertools import count


def part1(inp):
    grid = [[int(c) for c in l] for l in inp.splitlines()]
    nr, nc = len(grid), len(grid[0])
    cnt = count()
    q = [(0, next(cnt), 0, None, 0)]
    visited = set()
    while q:
        score, _, pos, streak_dir, streak_len = heappop(q)

        if pos == complex(nc - 1, nr - 1):
            return score
        if streak_len == 0:
            dirs = {1, -1, 1j, -1j}
        elif streak_len in (1, 2):
            dirs = {1, -1, 1j, -1j} - {-streak_dir}
        else:
            dirs = {1, -1, 1j, -1j} - {-streak_dir, streak_dir}
        for dir in dirs:
            new_pos = pos + dir
            if not (0 <= new_pos.imag < nr and 0 <= new_pos.real < nc):
                continue
            new_streak_len = (
                (streak_len + 1) if ((not streak_len) or streak_dir == dir) else 1
            )
            if (new_pos, dir, new_streak_len) in visited:
                continue
            visited.add((new_pos, dir, new_streak_len))
            heappush(
                q,
                (
                    score + grid[int(new_pos.imag)][int(new_pos.real)],
                    next(cnt),
                    new_pos,
                    dir,
                    new_streak_len,
                ),
            )


def part2(inp):
    grid = [[int(c) for c in l] for l in inp.splitlines()]
    nr, nc = len(grid), len(grid[0])
    cnt = count()
    q = [(0, next(cnt), 0, None, 0)]
    visited = set()
    while q:
        score, _, pos, streak_dir, streak_len = heappop(q)

        if pos == complex(nc - 1, nr - 1):
            return score
        if streak_len == 0:
            dirs = {1, -1, 1j, -1j}
        elif streak_len in (1, 2, 3):
            dirs = {streak_dir}
        elif streak_len in (4, 5, 6, 7, 8, 9):
            dirs = {1, -1, 1j, -1j} - {-streak_dir}
        else:
            dirs = {1, -1, 1j, -1j} - {-streak_dir, streak_dir}
        for dir in dirs:
            new_pos = pos + dir
            if not (0 <= new_pos.imag < nr and 0 <= new_pos.real < nc):
                continue
            new_streak_len = (
                (streak_len + 1) if ((not streak_len) or streak_dir == dir) else 1
            )
            if (new_pos, dir, new_streak_len) in visited:
                continue
            visited.add((new_pos, dir, new_streak_len))
            heappush(
                q,
                (
                    score + grid[int(new_pos.imag)][int(new_pos.real)],
                    next(cnt),
                    new_pos,
                    dir,
                    new_streak_len,
                ),
            )


def main():
    with open("input.txt") as f:
        inp = f.read()
    print(part1(inp))
    print(part2(inp))


main()
