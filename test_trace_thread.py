def matmul_list():
    import random

    random.seed(1)
    matrix_size = 300
    matrix_A = [[random.randint(1, 500) for _ in range(matrix_size)] for _ in range(matrix_size)]
    matrix_B = [[random.randint(1, 500) for _ in range(matrix_size)] for _ in range(matrix_size)]
    def matrix_multiply(A, B):
        result = [[0 for _ in range(matrix_size)] for _ in range(matrix_size)]
        for i in range(matrix_size):
            for j in range(matrix_size):
                for k in range(matrix_size):
                    # result[i][j] += A[i][j] * B[j][k] # upper
                    # result[i][j] += A[j][k] * B[k][i] # whole
                    result[i][j] += A[i][k] * B[k][j] # lower
        elem_resultA = [[id(element) for element in row] for row in matrix_A]
        flat_resultA = [id_value for row in elem_resultA for id_value in row]
        print("A: min: {}, max: {}".format(min(flat_resultA), max(flat_resultA)))

        elem_resultB = [[id(element) for element in row] for row in matrix_B]
        flat_resultB = [id_value for row in elem_resultB for id_value in row]
        print("B: min: {}, max: {}".format(min(flat_resultB), max(flat_resultB)))

        elem_result = [[id(element) for element in row] for row in result]
        flat_result = [id_value for row in elem_result for id_value in row]
        print("result: min: {}, max: {}".format(min(flat_result), max(flat_result)))
        # time.sleep(3)

    if do_bk == "True":
        # gc_count_module.start_count_gc_list(500000, 5120, "/home/lyuze/workspace/obj_heats/{}.txt".format(func), 1, 100)
        threadmodule.start_thread(500000, 102400, "/home/lyuze/workspace/obj_heats/{}.txt".format(func), 1, 100)
    start = time.time()
    matrix_multiply(matrix_A, matrix_B)
    latency = time.time() - start
    # time.sleep(3) # sleep here to make sure result does not get GC-ed + able to see max RSS
    print("latency: ", latency)

def matmul_numpy():
    import numpy as np
    n = 2000
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
    A = random.random_sample((n, n)) - 0.5
    B = A.sum(axis=1)
    A = matrix(A)
    B = matrix(B.reshape((n, 1)))
    start = time.time()
    print("start calculating at:", time.monotonic())
    x = linalg.solve(A, B)
    latency = time.time() - start
    print("latency: ", latency)

import gc, time, os, sys
gc.collect()
func = sys.argv[1]
do_bk = sys.argv[2]
os.environ["OPENBLAS_NUM_THREADS"] = "1"
os.environ["MKL_NUM_THREADS"] = "1"
if do_bk == "True":
    import threadmodule
    # import gc_count_module

if func == "matmul_list":
    matmul_list()
elif func == "matmul_np":
    matmul_numpy()
elif func == "linpack_np":
    linpack_numpy()
elif func == "sleep":
    time.sleep(6)
elif func == "complex":
    import complex_
    complex_.complex_func()
else:
    print("wrong workload")
    exit(1)
if do_bk == "True":
    threadmodule.close_thread()
    # gc_count_module.close_count_gc_list()
time.sleep(1)
print("all finished")
