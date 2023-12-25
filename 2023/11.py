#%%
def read_input() -> list[list[str]]:
    with open("inputs/11.txt") as f:
        return [list(line.strip()) for line in f.readlines()]

def expand_empty_lines(input: list[list[str]], factor: int) -> list[list[str]]:
    # if a line consists only of dots, duplicate it
    result = []
    for line in input:
        if all(c == '.' for c in line):
            result.extend([line] * factor)
        else:
            result.append(line)
    return result

def transpose(input: list[list[str]]) -> list[list[str]]:
    return list(map(list, zip(*input)))

def expand_empty_columns(input: list[list[str]], factor: int) -> list[list[str]]:
    # if a column consists only of dots, duplicate it
    return transpose(expand_empty_lines(transpose(input), factor))

def expand(input: list[list[str]], factor: int) -> list[list[str]]:
    # duplicate empty lines and columns
    return expand_empty_columns(expand_empty_lines(input, factor), factor)

def extract_object_coords(input: list[list[str]]) -> list[tuple[int, int]]:
    return [(x, y) for y, line in enumerate(input) for x, c in enumerate(line) if c == '#']

def all_pairs(xs: list[tuple[int, int]]) -> list[tuple[tuple[int, int], tuple[int, int]]]:
    return [(x, y) for i, x in enumerate(xs) for y in xs[i + 1:]]

def distance(x: tuple[int, int], y: tuple[int, int]) -> int:
    return abs(x[0] - y[0]) + abs(x[1] - y[1])

def recompute_distance_via_remainder(dist: int, f1: int, f2: int) -> int:
    original = dist % f1
    num_expansions = dist // f1
    return num_expansions * f2 + original

def solve(input: list[list[str]], factor: int) -> list[int]:
    expanded = expand(input, factor)
    coords = extract_object_coords(expanded)
    pairs = all_pairs(coords)
    return [distance(x, y) for x, y in pairs]

#%%
if __name__ == "__main__":
    inp = read_input()
    print(sum(solve(inp, 2)))
    dummy_factor = 300
    true_factor = 1_000_000
    res2 = solve(inp, dummy_factor)
    x = [recompute_distance_via_remainder(d, dummy_factor, true_factor) for d in res2]
    print(sum(x))

# %%
