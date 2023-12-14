# %%
CONNECTIONS = {
    '|': ((0, 1), (0, -1)),
    '-': ((1, 0), (-1, 0)),
    'L': ((0, -1), (1, 0)),
    'J': ((0, -1), (-1, 0)),
    '7': ((0, 1), (-1, 0)),
    'F': ((0, 1), (1, 0))
}

type Grid = list[list[str]]
type Step = tuple[int, int, int, int]
type Path = list[tuple[int, int]]
type Point = tuple[int, int]

def next(grid: Grid, x: int, y: int, dx: int, dy: int) -> Step:
    nx = x + dx
    ny = y + dy
    conn = CONNECTIONS[grid[ny][nx]]
    if conn[0] == (-dx, -dy):
        ndx, ndy = conn[1]
    elif conn[1] == (-dx, -dy):
        ndx, ndy = conn[0]
    else:
        raise Exception("Not connected")
    return nx, ny, ndx, ndy

def find_start(grid: Grid) -> Point:
    for y, row in enumerate(grid):
        for x, point in enumerate(row):
            if point == 'S':
                return x, y
    raise Exception("No starting point")

def find_first_step(grid: Grid, x: int, y: int) -> Point:
    for dx in (-1, 0, 1):
        for dy in (-1, 0, 1):
            if dx == dy == 0:
                continue
            try:
                next(grid, x, y, dx, dy)
                return dx, dy
            except:
                pass
    raise Exception("No first step")

def path(grid: Grid) -> Path:
    x, y = find_start(grid)
    dx, dy = find_first_step(grid, x, y)
    path = [(x, y)]
    while grid[y + dy][x + dx] != 'S':
        x, y, dx, dy = next(grid, x, y, dx, dy)
        path.append((x, y))
    return path

def solve(grid: Grid) -> int:
    return len(path(grid)) // 2

def read_grid() -> Grid:
    with open("inputs/10.txt") as f:
        return [list(line) for line in f]

if __name__ == "__main__":
    grid = read_grid()
    print(solve(grid))
# %%
