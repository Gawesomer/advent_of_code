from functools import lru_cache
from itertools import permutations

dice_permutations = list(set([i for i in permutations([1, 1, 1, 2, 2, 2, 3, 3, 3], 3)]))

@lru_cache(maxsize=21*21*10*10)
def wins_losses(p1, p2, s1, s2):
    if s2 >= 21:
        return 0, 1
    player1_wins = 0
    player2_wins = 0
    for rolls1 in dice_permutations:
        new_p1 = (p1 + sum(rolls1)) % 10
        new_s1 = s1 + new_p1 + 1
        if new_s1 >= 21:
            player1_wins += 1
        else:
            for rolls2 in dice_permutations:
                new_p2 = (p2 + sum(rolls2)) % 10
                new_s2 = s2 + new_p2 + 1
                wins = wins_losses(new_p1, new_p2, new_s1, new_s2)
                player1_wins += wins[0]
                player2_wins += wins[1]
    return player1_wins, player2_wins

print(wins_losses(7, 3, 0, 0))
