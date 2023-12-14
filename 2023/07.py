#%%
from collections import Counter

sample_input: list[str] = """32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
""".splitlines()

TYPE_ORDER: dict[tuple, int] = {
    (5,): 7,
    (4, 1): 6,
    (3, 2): 5,
    (3, 1, 1): 4,
    (2, 2, 1): 3,
    (2, 1, 1, 1): 2,
    (1, 1, 1, 1, 1): 1
}

CARD_ORDER = {
    "A": 14,
    "K": 13,
    "Q": 12,
    "J": 11,
    "T": 10,
    "9": 9,
    "8": 8,
    "7": 7,
    "6": 6,
    "5": 5,
    "4": 4,
    "3": 3,
    "2": 2
}

CARD_ORDER_JOKER = CARD_ORDER | {"J": 1}

def parse(s: list[str]) -> list[tuple[str, int]]:
    return [(a, int(b)) for l in s for a, b in [l.split()]]

def card_type(card: str) -> list[int]:
    counts = Counter(card)
    return list(sorted(counts.values(), reverse=True))

def card_type_joker(s: str) -> list[int]:
    joker_count = s.count("J")
    without_joker = s.replace("J", "")
    t = card_type(without_joker) if without_joker else [0]
    t[0] += joker_count
    return t

def type_key(card: str, to_card_type) -> int:
    return TYPE_ORDER[tuple(to_card_type(card))]

def card_key(card: str, order) -> tuple:
    return tuple(order[c] for c in card)

def key(card: str, to_card_type, card_order) -> tuple:
    return (type_key(card, to_card_type), card_key(card, card_order))

def key1(card: str) -> tuple:
    return key(card, card_type, CARD_ORDER)

def key2(card: str) -> tuple:
    return key(card, card_type_joker, CARD_ORDER_JOKER)

def fst(t: tuple):
    return t[0]

def solve(inputs: list[tuple[str, int]], key) -> int:
    inp_sorted = sorted(inputs, key=lambda x: key(fst(x)))
    ranked = [i*rank for i, (_, rank) in enumerate(inp_sorted, start=1)]
    return sum(ranked)

# %%
if __name__ == "__main__":
    with open("inputs/07.txt") as f:
        # lines = sample_input
        lines = f.readlines()
    parsed = parse(lines)
    print(solve(parsed, key1))
    print(solve(parsed, key2))

# %%
