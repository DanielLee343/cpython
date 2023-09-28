import gc_count_module, gc, time
gc.collect()

func = "obj_dump"

# workloads start here
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

gc_count_module.start_count_gc_list(500000, 5120, "/home/lyuze/workspace/obj_heats/{}.txt".format(func), 1, -1)
start = time.time()
matrix_multiply(matrix_A, matrix_B)
latency = time.time() - start
print("latency: ", latency)
gc_count_module.close_count_gc_list()
time.sleep(1)
