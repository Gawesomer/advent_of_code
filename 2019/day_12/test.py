import io
import unittest

from day_12.code import *
from day_12.moon import Moon


class MoonTestCaseHelper(unittest.TestCase):

    def assert_moons_equal(self, moon1, moon2):
        self.assertEqual(moon1.position.x, moon2.position.x)
        self.assertEqual(moon1.position.y, moon2.position.y)
        self.assertEqual(moon1.position.z, moon2.position.z)

        self.assertEqual(moon1.velocity.x, moon2.velocity.x)
        self.assertEqual(moon1.velocity.y, moon2.velocity.y)
        self.assertEqual(moon1.velocity.z, moon2.velocity.z)


class TestParser(MoonTestCaseHelper):

    def setUp(self):
        pass

    def test_parse_single_moon_all_positive_values(self):
        s = "<x=1, y=0, z=2>\n"
        input_file = io.StringIO(s)
        expected_moons = [
            Moon(1, 0, 2),
        ]

        actual_moons = parse_moons(input_file)

        for expected_moon, actual_moon in zip(expected_moons, actual_moons):
            self.assert_moons_equal(expected_moon, actual_moon)

    def test_parse_single_moon_negative_values(self):
        s = "<x=-1, y=-3, z=-2>\n"
        input_file = io.StringIO(s)
        expected_moons = [
            Moon(-1, -3, -2),
        ]

        actual_moons = parse_moons(input_file)

        for expected_moon, actual_moon in zip(expected_moons, actual_moons):
            self.assert_moons_equal(expected_moon, actual_moon)

    def test_parse_nominal_case(self):
        s = "<x=-1, y=0, z=2>\n" \
            "<x=2, y=-10, z=-7>\n" \
            "<x=4, y=-8, z=8>\n" \
            "<x=3, y=5, z=-1>\n"
        input_file = io.StringIO(s)
        expected_moons = [
            Moon(-1, 0, 2),
            Moon(2, -10, -7),
            Moon(4, -8, 8),
            Moon(3, 5, -1),
        ]

        actual_moons = parse_moons(input_file)

        for expected_moon, actual_moon in zip(expected_moons, actual_moons):
            self.assert_moons_equal(expected_moon, actual_moon)


class TestGravity(MoonTestCaseHelper):

    def setUp(self):
        pass

    def test_pull_nominal_case(self):
        moon1 = Moon(1, 0, 2)
        moon2 = Moon(-1, 1, 2)

        expected_moon1 = Moon(1, 0, 2, -1, 1, 0)

        moon1.pull(moon2)

        self.assert_moons_equal(expected_moon1, moon1)

    def test_apply_gravity_nominal_case(self):
        moons = [
            Moon(-1, 0, 2),
            Moon(2, -10, -7),
            Moon(4, -8, 8),
            Moon(3, 5, -1),
        ]
        expected_moons = [
            Moon(-1, 0, 2, 3, -1, -1),
            Moon(2, -10, -7, 1, 3, 3),
            Moon(4, -8, 8, -3, 1, -3),
            Moon(3, 5, -1, -1, -3, 1),
        ]

        apply_gravity(moons)

        for expected_moon, actual_moon in zip(expected_moons, moons):
            self.assert_moons_equal(expected_moon, actual_moon)


class TestVelocity(MoonTestCaseHelper):

    def setUp(self):
        pass

    def test_move_nominal_case(self):
        moon = Moon(1, 0, 2, -1, 1, 0)
        expected_moon = Moon(0, 1, 2, -1, 1, 0)

        moon.move()

        self.assert_moons_equal(expected_moon, moon)


class TestEnergy(MoonTestCaseHelper):

    def setUp(self):
        pass

    def test_potential_energy_nominal_case(self):
        moon = Moon(2, 1, -3, -3, -2, 1)

        self.assertEqual(moon.potential_energy, 6)

    def test_kinetic_energy_nominal_case(self):
        moon = Moon(2, 1, -3, -3, -2, 1)

        self.assertEqual(moon.kinetic_energy, 6)

    def test_total_energy_nominal_case(self):
        moon = Moon(2, 1, -3, -3, -2, 1)

        self.assertEqual(moon.total_energy, 36)


class TestSimulation(MoonTestCaseHelper):

    def setUp(self):
        pass

    def test_step_nominal_case(self):
        moons = [
            Moon(-1, 0, 2),
            Moon(2, -10, -7),
            Moon(4, -8, 8),
            Moon(3, 5, -1),
        ]
        expected_moons = [
            Moon(2, 1, -3, -3, -2, 1),
            Moon(1, -8, 0, -1, 1, 3),
            Moon(3, -6, 1, 3, 2, -3),
            Moon(2, 0, 4, 1, -1, -1),
        ]

        step(moons, 10)

        for expected_moon, actual_moon in zip(expected_moons, moons):
            self.assert_moons_equal(expected_moon, actual_moon)


if __name__ == "__main__":
    unittest.main()
