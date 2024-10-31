import math

# Quantum state as a function of time
def quantum_state(t):
    return math.cos(t)

# Decoherence function with a decay constant tau
def decoherence(psi, t, tau=0.5):
    return psi(t) * math.exp(-t / tau)

def test_quantum_decoherence():
    psi = quantum_state
    decohered_state = decoherence(psi, 2.0)
    print(f"Decohered state at t=2: {decohered_state}")
