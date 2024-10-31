# Representing qubit as a tuple (amplitude, phase)
def correct_qubit(qubit):
    amp, phase = qubit
    return (1.0, phase) if amp > 0.9 else qubit

# Function to correct a list of qubits
def error_correction(qubits):
    return list(map(correct_qubit, qubits))

def test_quantum_error_correction():
    qubits = [(0.8, 0.5), (0.95, 0.3), (1.0, 0.0)]
    corrected_qubits = error_correction(qubits)
    print(f"Corrected Qubits: {corrected_qubits}")
