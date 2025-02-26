import numpy as np
import matplotlib.pyplot as plt


# A simpler Hopfield Network implementation for beginners
class SimpleHopfieldNetwork:
    def __init__(self, size):
        """
        Initialize a simple Hopfield Network

        Parameters:
        size (int): Total number of neurons in the network
        """
        self.size = size
        self.weights = np.zeros((size, size))  # Start with all weights at zero

    def train(self, patterns):
        """
        Train the network on a list of patterns

        Parameters:
        patterns (list): List of patterns to memorize (each pattern should be a flat array of +1/-1)
        """
        # Reset weights to zero before training
        self.weights = np.zeros((self.size, self.size))

        # For each pattern we want to memorize
        for pattern in patterns:
            # The outer product creates a matrix where each element is xi * xj
            # This implements the Hebbian learning rule: neurons that fire together, wire together
            weight_changes = np.outer(pattern, pattern)

            # We don't want neurons to connect to themselves, so set diagonal to 0
            np.fill_diagonal(weight_changes, 0)

            # Add these weight changes to our overall weight matrix
            self.weights += weight_changes

        # Divide by the number of neurons to normalize the weights
        self.weights = self.weights / self.size

    def update_neuron(self, state, neuron_index):
        """
        Update a single neuron based on current network state

        Parameters:
        state (numpy array): Current state of the network
        neuron_index (int): Index of the neuron to update

        Returns:
        int: New state of the neuron (+1 or -1)
        """
        # Calculate the input to this neuron from all other neurons
        # This is just the dot product of the weights for this neuron and the current state
        activation = np.dot(self.weights[neuron_index], state)

        # Apply the threshold function: if activation > 0, return +1, else return -1
        return 1 if activation > 0 else -1

    def recall(self, initial_state, max_iterations=20):
        """
        Recall a pattern by running the network until it stabilizes

        Parameters:
        initial_state (numpy array): Starting state of the network
        max_iterations (int): Maximum number of updates to perform

        Returns:
        numpy array: The final state of the network
        int: Number of iterations performed
        """
        current_state = initial_state.copy()

        # Keep track of whether the state has changed
        iterations = 0

        # Continue until max iterations reached
        for i in range(max_iterations):
            iterations += 1
            old_state = current_state.copy()

            # Update each neuron in a random order (asynchronous update)
            # This is more natural and prevents oscillations
            update_order = np.random.permutation(self.size)
            for neuron_index in update_order:
                current_state[neuron_index] = self.update_neuron(current_state, neuron_index)

            # If the state didn't change after updating all neurons, we've converged
            if np.array_equal(old_state, current_state):
                break

        return current_state, iterations


# Helper functions to make the code easier to use

def create_random_pattern(size, square=True):
    """
    Create a random pattern of +1/-1 values

    Parameters:
    size (int): Size of the pattern (total number of elements)
    square (bool): If True, creates a square 2D pattern, otherwise creates a 1D pattern

    Returns:
    numpy array: Random pattern
    """
    # Create a random array of +1 and -1
    if square:
        # Calculate the side length of the square
        side = int(np.sqrt(size))
        pattern = np.random.choice([-1, 1], size=(side, side))
    else:
        pattern = np.random.choice([-1, 1], size=size)

    return pattern


def flatten_pattern(pattern):
    """
    Flatten a 2D pattern into a 1D array

    Parameters:
    pattern (numpy array): 2D pattern

    Returns:
    numpy array: Flattened pattern
    """
    return pattern.flatten()


def reshape_pattern(pattern, side):
    """
    Reshape a 1D pattern back into a 2D square

    Parameters:
    pattern (numpy array): 1D pattern
    side (int): Side length of the square

    Returns:
    numpy array: Reshaped 2D pattern
    """
    return pattern.reshape(side, side)


def corrupt_pattern(pattern, noise_level):
    """
    Add noise to a pattern by flipping some bits

    Parameters:
    pattern (numpy array): Original pattern
    noise_level (float): Proportion of bits to flip (0.0 to 1.0)

    Returns:
    numpy array: Corrupted pattern
    """
    # Make a copy of the pattern
    corrupted = pattern.copy()

    # Calculate how many bits to flip
    size = len(pattern)
    num_bits_to_flip = int(size * noise_level)

    # Choose random positions to flip
    positions_to_flip = np.random.choice(size, num_bits_to_flip, replace=False)

    # Flip the bits (multiply by -1)
    corrupted[positions_to_flip] *= -1

    return corrupted


def display_pattern(pattern, title=""):
    """
    Display a pattern as an image

    Parameters:
    pattern (numpy array): 2D pattern to display
    title (str): Title for the plot
    """
    plt.figure(figsize=(4, 4))
    plt.imshow(pattern, cmap='binary')
    plt.title(title)
    plt.axis('off')
    plt.tight_layout()
    plt.show()


def calculate_accuracy(original, recalled):
    """
    Calculate how similar two patterns are

    Parameters:
    original (numpy array): Original pattern
    recalled (numpy array): Recalled pattern

    Returns:
    float: Percentage of matching elements
    """
    # Count how many elements match
    matches = np.sum(original == recalled)
    total = len(original)

    # Return as a percentage
    return (matches / total) * 100


# Main function to run the experiment
def main():
    # Set a random seed for reproducibility
    np.random.seed(123)

    # Parameters
    pattern_side = 10  # Size of each side of our square pattern
    pattern_size = pattern_side * pattern_side  # Total number of neurons
    num_patterns = 3  # Number of patterns to store

    print(f"Creating a Hopfield Network with {pattern_size} neurons")
    print(f"Storing {num_patterns} patterns of size {pattern_side}x{pattern_side}")

    # Create random patterns
    patterns_2d = [create_random_pattern(pattern_size, square=True) for _ in range(num_patterns)]

    # Flatten patterns for the network
    patterns_flat = [flatten_pattern(p) for p in patterns_2d]

    # Create and train our network
    network = SimpleHopfieldNetwork(pattern_size)
    network.train(patterns_flat)

    print("\nTesting recall of original patterns:")
    # Test recall of original patterns
    for i, pattern in enumerate(patterns_flat):
        recalled, iterations = network.recall(pattern)
        accuracy = calculate_accuracy(pattern, recalled)
        print(f"Pattern {i + 1}: Accuracy = {accuracy:.1f}%, Iterations = {iterations}")

    # Test with different noise levels
    noise_levels = [0.1, 0.2, 0.3, 0.4, 0.5]

    print("\nTesting recall with corrupted patterns:")
    for noise in noise_levels:
        print(f"\nNoise level: {noise * 100:.0f}%")

        # Test each pattern
        for i, pattern in enumerate(patterns_flat):
            # Corrupt the pattern
            corrupted = corrupt_pattern(pattern, noise)

            # Try to recall the original
            recalled, iterations = network.recall(corrupted)

            # Calculate accuracy
            accuracy = calculate_accuracy(pattern, recalled)
            print(f"Pattern {i + 1}: Accuracy = {accuracy:.1f}%, Iterations = {iterations}")

            # For the first pattern, show a visual example
            if i == 0:
                # Reshape patterns for display
                original_2d = reshape_pattern(pattern, pattern_side)
                corrupted_2d = reshape_pattern(corrupted, pattern_side)
                recalled_2d = reshape_pattern(recalled, pattern_side)

                # Display the patterns
                plt.figure(figsize=(12, 4))

                plt.subplot(1, 3, 1)
                plt.imshow(original_2d, cmap='binary')
                plt.title("Original Pattern")
                plt.axis('off')

                plt.subplot(1, 3, 2)
                plt.imshow(corrupted_2d, cmap='binary')
                plt.title(f"Corrupted ({noise * 100:.0f}% noise)")
                plt.axis('off')

                plt.subplot(1, 3, 3)
                plt.imshow(recalled_2d, cmap='binary')
                plt.title(f"Recalled (Accuracy: {accuracy:.1f}%)")
                plt.axis('off')

                plt.tight_layout()
                plt.show()


# Run the main function if this script is executed
if __name__ == "__main__":
    main()
