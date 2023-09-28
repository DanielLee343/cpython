#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdint.h>

// static PyObject *global_var = NULL;
volatile short terminate_flag = 0;
FILE *output_fd = NULL;

static void thread_func(void *arg){
    output_fd = (FILE*)arg;
    while(!terminate_flag){
        PyGILState_STATE gstate = PyGILState_Ensure();
        // struct timespec ts;
        // clock_gettime(CLOCK_REALTIME, &ts);
        // fprintf(output_fd, "%ld.%ld\n", ts.tv_sec, ts.tv_nsec);
        // fflush(output_fd);
        Py_ssize_t gen = 0;
        intptr_t ptrint_extended = (intptr_t)gc_get_objects_impl_no_mod(gen);
        intptr_t ptrint_masked = ptrint_extended & 0x00007FFFFFFFFFFF; // have to do this 

        PyObject * result = (PyObject *)ptrint_masked;
        Py_ssize_t cur_size = PyList_Size(result);
        fprintf(output_fd, "Size of list %zd: %zd\n", gen, cur_size);
        for (Py_ssize_t i = 0; i < cur_size; i++) {
            PyObject *item = PyList_GetItem(result, i);
            PyObject *repr = PyObject_Repr(item);
            if (repr) {
                fprintf(output_fd, "%s\n", PyUnicode_AsUTF8(repr));
                Py_DECREF(repr);
            }
        }
        Py_BEGIN_ALLOW_THREADS
            usleep(500000);
        Py_END_ALLOW_THREADS
        PyGILState_Release(gstate);
    }
    fprintf(stderr, "getout\n");
}

static PyObject *start_count_gc_list(PyObject *self, PyObject *args)
{
    // FILE *output_fd = fopen("/home/lyuze/workspace/obj_heats/obj_dump.txt", "w");
    unsigned int sample_dur;
    unsigned int buff_size;
    const char *file;
    unsigned int doIO;
    Py_ssize_t gen;
    if (!PyArg_ParseTuple(args, "ii|sin", &sample_dur, &buff_size, &file, &doIO, &gen))
    {
        return NULL; // error
    }
    fprintf(stderr, "file is %s, sample dur is %d\n", file, sample_dur);
    BookkeepArgs *bookkeepArgs = (BookkeepArgs *)malloc(sizeof(BookkeepArgs));
    output_fd = fopen(file, "w");
    if (output_fd == NULL)
    {
        perror("failed to open file\n");
    }
    bookkeepArgs->sample_dur = sample_dur;
    bookkeepArgs->fd = output_fd;
    bookkeepArgs->buff_size = buff_size;
    bookkeepArgs->doIO = doIO;
    bookkeepArgs->gen = gen;
    long thread_id = PyThread_start_new_thread(thread_trace_from_gc_list, (void*)bookkeepArgs);
    if (thread_id == -1)
    {
        return NULL; // error 
        perror("thread spawn error\n");
    }
    Py_RETURN_NONE;
}

static PyObject *close_count_gc_list(PyObject *self, PyObject *args)
{
    fprintf(stderr, "flush complete, closing fd\n");
    terminate_flag = 1;
    usleep(1000000);
    fclose(output_fd);
    Py_RETURN_NONE;
}

static PyMethodDef ThreadMethods_[] = {
    {"start_count_gc_list", start_count_gc_list, METH_VARARGS, "Start a thread counting from gc list."},
    {"close_count_gc_list", close_count_gc_list, METH_NOARGS, "Close the counting thread."},
    {NULL, NULL, 0, NULL}};

static struct PyModuleDef gc_count_module = {
    PyModuleDef_HEAD_INIT,
    "gc_count_module",
    NULL,
    -1,
    ThreadMethods_};

PyMODINIT_FUNC PyInit_gc_count_module(void)
{
    return PyModule_Create(&gc_count_module);
}
