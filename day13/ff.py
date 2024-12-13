def find_button_presses(a_x, a_y, b_x, b_y, target_x, target_y):
    # Initialize a large number for iterations to avoid infinite loops
    max_iterations = 1000

    # Iterate over possible values for n_A and n_B
    for n_A in range(max_iterations):
        for n_B in range(max_iterations):
            # Calculate the resulting position
            current_x = n_A * a_x + n_B * b_x
            current_y = n_A * a_y + n_B * b_y

            # Check if the current position matches the target
            if current_x == target_x and current_y == target_y:
                return (n_A, n_B)

    return None  # Return None if no solution is found within the iteration limit

# Example usage
a_x, a_y = 94, 34  # Movement in X and Y for button A
b_x, b_y = 22, 67  # Movement in X and Y for button B
target_x, target_y = 8400, 5400  # Target position

solution = find_button_presses(a_x, a_y, b_x, b_y, target_x, target_y)
if solution:
    print("Number of presses for A and B:", solution)
else:
    print("No solution found within the iteration limit.")

