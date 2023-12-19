import re


def h(s):
    res = 0
    for x in s:
        res = ((res + ord(x)) * 17) % 256
    return res


def part1(inp):
    return sum(map(h, inp.strip().split(",")))


def part2(inp):
    buckets = [{} for _ in range(256)]
    for instr in inp.strip().split(","):
        if match := re.match(r"([a-z]+)\-", instr):
            label = match.group(1)
            box = buckets[h(label)]
            if label in box:
                box.pop(label)
        if match := re.match(r"([a-z]+)=(\d+)", instr):
            label = match.group(1)
            buckets[h(label)][label] = int(match.group(2))
    s = 0
    for i, bucket in enumerate(buckets, start=1):
        for j, foc in enumerate(bucket.values(), start=1):
            s += i * j * foc
    return s


def main():
    with open("input.txt") as f:
        inp = f.read()
    print(part1(inp))
    print(part2(inp))


main()
