import io
import unittest

from day_17.code import *


class TestParser(unittest.TestCase):

    def setUp(self):
        pass

    def test_parse_input_nominal_case(self):
        s = ".#.\n" \
            "..#\n" \
            "###\n"
        input_file = io.StringIO(s)
        expected_dimension = {
            0: {
                0: {0: '.', 1: '#', 2: '.',},
                1: {0: '.', 1: '.', 2: '#',},
                2: {0: '#', 1: '#', 2: '#',},
                },
        }
        dimension = parse_input(input_file)

        self.assertEqual(expected_dimension, dimension)


class TestProcessor(unittest.TestCase):

    def setUp(self):
        pass

    def test_get_position_in_dimension(self):
        dimension = {
            0: {
                0: {0: '.', 1: '#', 2: '.',},
                1: {0: '.', 1: '.', 2: '#',},
                2: {0: '#', 1: '#', 2: '#',},
                },
        }

        self.assertEqual(get_value(dimension, 2, 1, 0), '#')

    def test_get_position_outside_dimension(self):
        dimension = {
            0: {
                0: {0: '.', 1: '#', 2: '.',},
                1: {0: '.', 1: '.', 2: '#',},
                2: {0: '#', 1: '#', 2: '#',},
                },
        }

        self.assertEqual(get_value(dimension, 1, 1, 1), '.')

    def test_set_position_in_dimension(self):
        dimension = {
            0: {
                0: {0: '.', 1: '#', 2: '.',},
                1: {0: '.', 1: '.', 2: '#',},
                2: {0: '#', 1: '#', 2: '#',},
                },
        }
        expected_dimension = {
            0: {
                0: {0: '.', 1: '#', 2: '.',},
                1: {0: '.', 1: '#', 2: '#',},
                2: {0: '#', 1: '#', 2: '#',},
                },
        }

        set_value(dimension, 1, 1, 0, '#')

        self.assertEqual(expected_dimension, dimension)

    def test_set_position_outside_dimension(self):
        dimension = {
            0: {
                0: {0: '.', 1: '#', 2: '.',},
                1: {0: '.', 1: '.', 2: '#',},
                2: {0: '#', 1: '#', 2: '#',},
                },
        }
        expected_dimension = {
            0: {
                0: {0: '.', 1: '#', 2: '.',},
                1: {0: '.', 1: '.', 2: '#',},
                2: {0: '#', 1: '#', 2: '#',},
                },
            1: {
                1: {1: '#'},
                },
        }

        set_value(dimension, 1, 1, 1, '#')

        self.assertEqual(expected_dimension, dimension)

    def test_get_all_actives_nominal_case(self):
        dimension = {
            0: {
                0: {0: '.', 1: '#', 2: '.',},
                1: {0: '.', 1: '.', 2: '#',},
                2: {0: '#', 1: '#', 2: '#',},
                },
        }
        expected_actives = [
            [1, 0, 0],
            [2, 1, 0],
            [0, 2, 0],
            [1, 2, 0],
            [2, 2, 0],
        ]

        self.assertEqual(get_all_actives(dimension), expected_actives)

    def test_get_neighbours_indeces(self):
        self.assertEqual(len(get_neighbours_indeces(0, 0, 0)), 26)

    def test_split_active_inactive_nominal_case(self):
        dimension = {
            0: {
                0: {0: '.', 1: '#', 2: '.',},
                1: {0: '.', 1: '.', 2: '#',},
                2: {0: '#', 1: '#', 2: '#',},
                },
        }
        positions = [
            [1, 1, 0],
            [2, 1, 0],
            [1, 2, 0],
            [2, 2, 0],
        ]
        expected_actives = [
            [2, 1, 0],
            [1, 2, 0],
            [2, 2, 0],
        ]
        expected_inactives = [
            [1, 1, 0],
        ]

        actives, inactives = split_active_inactive(dimension, positions)

        self.assertEqual(expected_actives, actives)
        self.assertEqual(expected_inactives, inactives)

    def test_get_num_active_neighbours_nominal(self):
        dimension = {
            0: {
                0: {0: '.', 1: '#', 2: '.',},
                1: {0: '.', 1: '.', 2: '#',},
                2: {0: '#', 1: '#', 2: '#',},
                },
        }

        self.assertEqual(get_num_active_neighbours(dimension, 1, 1, 0), 5)

    def test_activate_deactivate_nominal_case(self):
        dimension = {
            0: {
                0: {0: '.', 1: '#', 2: '.',},
                1: {0: '.', 1: '.', 2: '#',},
                2: {0: '#', 1: '#', 2: '#',},
                },
        }

        actives = get_all_actives(dimension)
        will_activate = to_activate(dimension, actives)
        will_deactivate = to_deactivate(dimension, actives)

        print("{}, {}".format(len(will_activate), len(will_deactivate)))

    def test_simulate_nominal_case(self):
        dimension = {
            0: {
                0: {0: '.', 1: '#', 2: '.',},
                1: {0: '.', 1: '.', 2: '#',},
                2: {0: '#', 1: '#', 2: '#',},
                },
        }

        for i in range(6):
            dimension = simulate(dimension)

        self.assertEqual(len(get_all_actives(dimension)), 112)


if __name__ == "__main__":
    unittest.main()
