# %%
from typing import Callable, Iterable
import re

samle_input_1 = """1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
"""

samle_input_2 = """two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
"""

digits = dict(
    one=1,
    two=2,
    three=3,
    four=4,
    five=5,
    six=6,
    seven=7,
    eight=8,
    nine=9
)
#%%

digits_reversed = {k[::-1]: v for k, v in digits.items()}

# %%

def first_digit(s: str, names: Iterable[str]) -> str:
    result = re.search(r"\d|" + "|".join(names), s)
    if result is None:
        raise ValueError(f"Could not find any of {names} in {s}")
    else:
        return result.group()

def value_1(s: str) -> int:
    ds = [int(c) for c in s if c.isdigit()]
    return 10 * ds[0] + ds[-1]

def translate_digit(s: str) -> int:
    if s.isdigit():
        return int(s)
    else:
        return (digits | digits_reversed)[s]

def value_2(s: str) -> int:
    first = first_digit(s, digits.keys())
    last = first_digit(s[::-1], digits_reversed.keys())
    return 10* translate_digit(first) + translate_digit(last)

def solve(s: str, f: Callable[[str], int]) -> int:
    return sum(map(f, s.splitlines()))

def main():
    print(solve(samle_input_1, value_1))
    print(solve(samle_input_2, value_2))
    with open("inputs/01.txt") as f:
        c = f.read()
        print(solve(c, value_1))
        print(solve(c, value_2))

#%%
if __name__ == "__main__":
    main()