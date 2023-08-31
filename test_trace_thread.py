def matmul_list():
    import random

    matrix_size = 300
    matrix_A = [[random.randint(1, 10) for _ in range(matrix_size)] for _ in range(matrix_size)]
    matrix_B = [[random.randint(1, 10) for _ in range(matrix_size)] for _ in range(matrix_size)]

    def matrix_multiply(A, B):
        result = [[0 for _ in range(matrix_size)] for _ in range(matrix_size)]
        for i in range(matrix_size):
            for j in range(matrix_size):
                for k in range(matrix_size):
                    result[i][j] += A[i][k] * B[k][j]
        return result
    start = time.time()
    matrix_multiply(matrix_A, matrix_B)
    latency = time.time() - start
    print("latency: ", latency)

def matmul_numpy():
    import numpy as np
    n = 2000
    # int(sys.argv[1])
    A = np.random.rand(n, n)
    B = np.random.rand(n, n)

    start = time.time()
    print("start calculating at:", time.monotonic())
    C = np.matmul(A, B)
    latency = time.time() - start
    print("latency: ", latency)

def linpack_numpy():
    n = 4500
    from numpy import matrix, linalg, random
    # LINPACK benchmarks
    ops = (2.0 * n) * n * n / 3.0 + (2.0 * n) * n

    # Create AxA array of random numbers -0.5 to 0.5
    A = random.random_sample((n, n)) - 0.5
    B = A.sum(axis=1)

    # Convert to matrices
    A = matrix(A)
    B = matrix(B.reshape((n, 1)))

    # Ax = B
    start = time.time()
    print("start calculating at:", time.monotonic())
    x = linalg.solve(A, B)
    latency = time.time() - start
    print("latency: ", latency)

    # mflops = (ops * 1e-6 / latency)

    # result = {
    #     'mflops': mflops,
    #     'latency': latency
    # }

    # return result

import time
# import os
# os.environ["OPENBLAS_NUM_THREADS"] = "1"
# os.environ["MKL_NUM_THREADS"] = "1"
import threadmodule
threadmodule.start_thread(100000, 10240, "/home/lyuze/workspace/obj_heats/matmul_list.txt", True)
matmul_list()
threadmodule.close_thread()
time.sleep(3)
print("all finished")
