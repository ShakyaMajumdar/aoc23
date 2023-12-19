from functools import cache


@cache
def calc_ways(is_op_must, is_dam_must, rcrd, grps):
    if rcrd == () and grps == ():
        return 1
    if rcrd == ():
        return 0
    r, *rs = rcrd
    rs = tuple(rs)
    if r == "." and is_dam_must:
        return 0
    if r == ".":
        return calc_ways(False, False, rs, grps)
    if r == "#" and is_op_must:
        return 0
    if r == "#" and not grps:
        return 0
    if r == "#" and grps[0] > 1:
        return calc_ways(False, True, rs, (grps[0] - 1, *grps[1:]))
    if r == "#" and grps[0] == 1:
        return calc_ways(True, False, rs, grps[1:])
    if r == "?" and is_dam_must:
        return calc_ways(is_op_must, is_dam_must, ("#",) + rs, grps)
    if r == "?" and is_op_must:
        return calc_ways(is_op_must, is_dam_must, (".",) + rs, grps)
    if r == "?":
        return calc_ways(is_op_must, is_dam_must, ("#",) + rs, grps) + calc_ways(
            is_op_must, is_dam_must, (".",) + rs, grps
        )


def main():
    with open("input.txt") as f:
        inp = f.read().splitlines()

    parsed2 = []
    for i in inp:
        x, y = i.split()
        x = ((tuple(x) + ("?",)) * 5)[:-1]
        y = tuple(map(int, y.split(","))) * 5
        parsed2.append((x, y))
    print(sum([calc_ways(False, False, r, g) for r, g in parsed2]))


main()
