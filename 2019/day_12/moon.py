class Moon(object):

    class Coordinate(object):
        def __init__(self, x, y, z):
            self.x = x
            self.y = y
            self.z = z

    def __init__(self, x=0, y=0, z=0, vx=0, vy=0, vz=0):
        self.position = self.Coordinate(x, y, z)
        self.velocity = self.Coordinate(vx, vy, vz)

    def pull(self, other_moon):
        """
        Pulls this moon towards `other_moon` by increasing velocity by one if
        position is smaller than `other_moon`'s, decreasing velocity by one if
        position is larger than `other_moon`'s, or leaving it unchanged if
        positions match (this is done for each axis)
        """
        if self.position.x < other_moon.position.x:
            self.velocity.x += 1
        elif self.position.x > other_moon.position.x:
            self.velocity.x -= 1

        if self.position.y < other_moon.position.y:
            self.velocity.y += 1
        elif self.position.y > other_moon.position.y:
            self.velocity.y -= 1

        if self.position.z < other_moon.position.z:
            self.velocity.z += 1
        elif self.position.z > other_moon.position.z:
            self.velocity.z -= 1

    def move(self):
        """
        Update moon's position by applying velocity once
        params:
            none
        returns:
            nothing
        side-effect:
            velocity was added to position
        """
        self.position.x += self.velocity.x
        self.position.y += self.velocity.y
        self.position.z += self.velocity.z

    @property
    def potential_energy(self):
        return (
            abs(self.position.x) + \
            abs(self.position.y) + \
            abs(self.position.z)
        )

    @property
    def kinetic_energy(self):
        return (
            abs(self.velocity.x) + \
            abs(self.velocity.y) + \
            abs(self.velocity.z)
        )

    @property
    def total_energy(self):
        return (self.potential_energy * self.kinetic_energy)

    def __str__(self):
        return "pos=<x={}, y={}, z={}>, " \
            "vel=<x={}, y={}, z={}>, " \
            "ngy=<p={}, k={}, t={}>".format(
            self.position.x, self.position.y, self.position.z,
            self.velocity.x, self.velocity.y, self.velocity.z,
            self.potential_energy, self.kinetic_energy, self.total_energy,
        )
