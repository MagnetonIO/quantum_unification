import math

def special_relativity_dilation(v, t):
    c2 = 299792458 ** 2  # Speed of light squared
    return t / math.sqrt(1 - (v ** 2 / c2))

def gravitational_dilation(phi, t):
    c2 = 299792458 ** 2
    return t * math.sqrt(1 - 2 * phi / c2)

def test_time_dilation():
    t = 1.0
    v = 100000
    phi = 9.8
    print(f"Special Relativity Dilation: {special_relativity_dilation(v, t)}")
    print(f"Gravitational Dilation: {gravitational_dilation(phi, t)}")
