class Sheaf:
    def __init__(self, sections):
        self.sections = sections

    def section_at(self, point):
        return next((v for p, v in self.sections if p == point), None)

    def fmap(self, func):
        return Sheaf([(p, func(v)) for p, v in self.sections])

def test_topos_theory():
    spacetime_sheaf = Sheaf([(0.0, "State1"), (1.0, "State2")])
    print(spacetime_sheaf.section_at(0.0))
