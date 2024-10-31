# Objects and morphisms in Category Theory
class Object:
    def __init__(self, name):
        self.name = name

class Morphism:
    def __init__(self, source, target, func):
        self.source = source
        self.target = target
        self.func = func

# Functor: a mapping between categories
class Functor:
    def __init__(self, fmap_obj, fmap_mor):
        self.fmap_obj = fmap_obj
        self.fmap_mor = fmap_mor

def identity(obj):
    return Morphism(obj, obj, lambda x: x)

def compose(m1, m2):
    return Morphism(m2.source, m1.target, lambda x: m1.func(m2.func(x)))

def test_category_theory():
    obj_a = Object("A")
    obj_b = Object("B")
    morphism = Morphism(obj_a, obj_b, lambda x: obj_b)
    functor = Functor(lambda o: o, lambda m: m)
    print(f"Functor maps object: {functor.fmap_obj(obj_a)}")
