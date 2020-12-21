import os
import pathlib


def parse_input(input_file):
    """
    Get foods from file
    params:
        input_file - file to parse, opened for reading
    returns:
        dict(id: list(str))
    """
    foodmap = []
    allergens = set()
    foods = set()
    for i, line in enumerate(input_file):
        foodmap.append([])
        parentheses_index = line.find('(')
        foodmap[i].append(set(line[:parentheses_index].strip().split()))
        foodmap[i].append(set(line[parentheses_index+10:len(line)-2].split(', ')))
        for a in foodmap[i][1]:
            allergens.add(a)
        for f in foodmap[i][0]:
            foods.add(f)
    return foodmap, allergens, foods


def allergen_to_foods(foodmap, allergens):
    """
    return:
        dict(allergen: list(set(food_item)))
    """
    res = {}
    for a in allergens:
        res[a] = []
        for f in foodmap:
            if a in f[1]:
                res[a].append(f[0])
    return res


def allergen_to_foodintersection(allergenfood_map):
    """
    side-effect:
        dict(allergen: set(intersection of sets)
    """
    for a, food_sets in allergenfood_map.items():
        if len(food_sets) == 0:
            continue
        xsection = food_sets[0]
        i = 1
        while i < len(food_sets):
            xsection = xsection.intersection(food_sets[i])
            i += 1
        allergenfood_map[a] = xsection


def find_nonallergenic_foods(allergenfood_intersection, foods):
    food_copy = foods.copy()
    for a, foodset in allergenfood_intersection.items():
        for f in foods:
            if f in foodset:
                food_copy.discard(f)
    return food_copy


if __name__ == "__main__":
    input_filename = os.path.join(pathlib.Path(__file__).parent, "input.txt")
    with open(input_filename, "r") as input_file:
        foodmap, allergens, foods = parse_input(input_file)
    allergenfood_map = allergen_to_foods(foodmap, allergens)
    allergen_to_foodintersection(allergenfood_map)
    nonallergenic = find_nonallergenic_foods(allergenfood_map, foods)
    res = 0
    for f in foodmap:
        for af in nonallergenic:
            if af in f[0]:
                res += 1
    print(res)
