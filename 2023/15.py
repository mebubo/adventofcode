#%%
from functools import reduce
from collections import OrderedDict as odict
from collections import defaultdict as ddict
from dataclasses import dataclass

def read_input() -> list[str]:
    with open("inputs/15.txt") as f:
        return f.read().strip().split(",")

def hash(curr: int, symbol: str) -> int:
    return (curr + ord(symbol)) * 17 % 256

def hash_str(symbols: str) -> int:
    return reduce(hash, symbols, 0)

def part1(input: list[str]) -> int:
    return sum(hash_str(s) for s in input)

@dataclass
class Add:
    label: str
    value: int

@dataclass
class Remove:
    label: str

type Instruction = Add | Remove

def parse_instruction(s: str) -> Instruction:
    if '-' in s:
        return Remove(s[:-1])
    else:
        l, v = s.split('=')
        return Add(l, int(v))

def perform_instruction(boxes: dict[int, dict[str, int]], instruction: Instruction) -> None:
    match instruction:
        case Add(label, value):
            box = boxes[hash_str(label)]
            box[label] = value
        case Remove(label):
            box = boxes[hash_str(label)]
            if label in box:
                del box[label]

def focusing_power(boxes: dict[int, dict[str, int]]) -> int:
    return sum(f * (b + 1) * (s + 1)
               for b in range(256)
               for s, (_, f) in enumerate(boxes[b].items()))

def part2(input: list[str]) -> int:
    boxes: dict[int, dict[str, int]] = ddict(odict)
    for s in input:
        perform_instruction(boxes, parse_instruction(s))
    return focusing_power(boxes)

if __name__ == "__main__":
    input = read_input()
    print(part1(input))
    print(part2(input))
# %%
