import io
import unittest

from day_21.code import *


class TestParser(unittest.TestCase):

    def setUp(self):
        pass

    def test_parse_input_nominal_case(self):
        s = "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)\n" \
            "trh fvjkl sbzzf mxmxvkd (contains dairy)\n" \
            "sqjhc fvjkl (contains soy)\n" \
            "sqjhc mxmxvkd sbzzf (contains fish)\n"
        input_file = io.StringIO(s)
        expected_foodmap = [
            [
                {"mxmxvkd", "kfcds", "sqjhc", "nhms"},
                {"dairy", "fish"},
            ],
            [
                {"trh", "fvjkl", "sbzzf", "mxmxvkd"},
                {"dairy"},
            ],
            [
                {"sqjhc", "fvjkl"},
                {"soy"},
            ],
            [
                {"sqjhc", "mxmxvkd", "sbzzf"},
                {"fish"},
            ],
        ]
        expected_allergens = {"dairy", "fish", "soy"}
        expected_foods = {"mxmxvkd", "kfcds", "sqjhc", "nhms", "trh", "fvjkl", "sbzzf"}

        foodmap, allergens, foods = parse_input(input_file)

        self.assertEqual(expected_foodmap, foodmap)
        self.assertEqual(expected_allergens, allergens)
        self.assertEqual(expected_foods, foods)


class TestProcessor(unittest.TestCase):

    def setUp(self):
        pass

    def test_allergen_to_foods_nominal_case(self):
        foods = [
            [
                {"mxmxvkd", "kfcds", "sqjhc", "nhms"},
                {"dairy", "fish"},
            ],
            [
                {"trh", "fvjkl", "sbzzf", "mxmxvkd"},
                {"dairy"},
            ],
            [
                {"sqjhc", "fvjkl"},
                {"soy"},
            ],
            [
                {"sqjhc", "mxmxvkd", "sbzzf"},
                {"fish"},
            ],
        ]
        allergens = {"dairy", "fish", "soy"}

        expected_allergenfood_map = {
            "dairy": [
                {"mxmxvkd", "kfcds", "sqjhc", "nhms"},
                {"trh", "fvjkl", "sbzzf", "mxmxvkd"},
            ],
            "fish": [
                {"mxmxvkd", "kfcds", "sqjhc", "nhms"},
                {"sqjhc", "mxmxvkd", "sbzzf"},
            ],
            "soy": [
                {"sqjhc", "fvjkl"},
            ],
        }

        allergenfood_map = allergen_to_foods(foods, allergens)

        self.assertEqual(expected_allergenfood_map, allergenfood_map)


    def test_allergen_to_foodintersection_nominal_case(self):
        allergenfood_map = {
            "dairy": [
                {"mxmxvkd", "kfcds", "sqjhc", "nhms"},
                {"trh", "fvjkl", "sbzzf", "mxmxvkd"},
            ],
            "fish": [
                {"mxmxvkd", "kfcds", "sqjhc", "nhms"},
                {"sqjhc", "mxmxvkd", "sbzzf"},
            ],
            "soy": [
                {"sqjhc", "fvjkl"},
            ],
        }
        expected_allergen_to_foodintersection = {
            "dairy": {"mxmxvkd"},
            "fish": {"mxmxvkd", "sqjhc"},
            "soy": {"sqjhc", "fvjkl"},
        }

        allergen_to_foodintersection(allergenfood_map)

        self.assertEqual(expected_allergen_to_foodintersection, allergenfood_map)

    def test_find_non_allergenic_foods_nominal_case(self):
        allergenfood_intersection = {
            "dairy": {"mxmxvkd"},
            "fish": {"mxmxvkd", "sqjhc"},
            "soy": {"sqjhc", "fvjkl"},
        }
        foods = {"mxmxvkd", "kfcds", "sqjhc", "nhms", "trh", "fvjkl", "sbzzf"}
        expected_nonallergenic = {"kfcds", "nhms", "sbzzf", "trh"}

        nonallergenic = find_nonallergenic_foods(allergenfood_intersection, foods)

        self.assertEqual(expected_nonallergenic, nonallergenic)


if __name__ == "__main__":
    unittest.main()
