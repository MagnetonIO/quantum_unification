from time_dilation import special_relativity_dilation, gravitational_dilation

# Lorentz transformation for special relativity
def lorentz_transformation(v, t):
    return special_relativity_dilation(v, t)

# Gravitational transformation for diffeomorphism
def gravitational_transformation(phi, t):
    return gravitational_dilation(phi, t)

# Combined representation of time dilation
def combined_representation(v, phi, t):
    return lorentz_transformation(v, gravitational_transformation(phi, t))

def test_representation_theory():
    t = 1.0        # Initial time
    v = 1.0e5      # Velocity
    phi = 9.8      # Gravitational potential
    result = combined_representation(v, phi, t)
    print(f"Combined time dilation result: {result}")
