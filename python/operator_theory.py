# Operator theory in Python
class Operator:
    def __init__(self, func):
        self.func = func

def apply_operator(op, psi, x):
    return op.func(psi(x))

def scale_operator(scalar):
    return Operator(lambda x: scalar * x)

def test_operator_theory():
    psi = lambda x: x * x
    op = scale_operator(2.0)
    print(apply_operator(op, psi, 3.0))
