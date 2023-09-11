import random
import time

# Approximate Pi using Monte Carlo method
def monte_carlo_pi(num_samples):
    inside_circle = 0

    for _ in range(num_samples):
        x, y = random.random(), random.random()
        distance = x ** 2 + y ** 2

        if distance <= 1:
            inside_circle += 1

    return (inside_circle / num_samples) * 4

# Calculate Fibonacci sequence using matrix exponentiation
def matrix_mult(a, b):
    n = len(a)
    m = len(a[0])
    p = len(b[0])
    result = [[0 for _ in range(p)] for _ in range(n)]

    for i in range(n):
        for j in range(p):
            for k in range(m):
                result[i][j] += a[i][k] * b[k][j]

    return result

def matrix_power(matrix, n):
    if n == 1:
        return matrix

    if n % 2:
        return matrix_mult(matrix, matrix_power(matrix, n - 1))

    half_pow = matrix_power(matrix, n // 2)
    return matrix_mult(half_pow, half_pow)

def fib(n):
    if n == 0:
        return 0

    base = [[1, 1], [1, 0]]
    result_matrix = matrix_power(base, n - 1)

    return result_matrix[0][0]

# Evaluate polynomial at multiple points using Horner's method
def horner_evaluate(coeffs, x_values):
    results = []
    
    for x in x_values:
        result = 0
        for coeff in reversed(coeffs):
            result = result * x + coeff
        results.append(result)

    return results

def complex_func():
    start_time = time.time()
    for i in range(8):
        pi_value = monte_carlo_pi(10000000)
        fib_value = fib(300)
        polynomial = [2.496, -3.208, 5.279, -2.765]
        x_values = [0, 1, 2, 3]
        y_values = horner_evaluate(polynomial, x_values)

    end_time = time.time()
    print(f"Time taken: {end_time - start_time} seconds")

# if __name__ == "__main__":
#     main()
