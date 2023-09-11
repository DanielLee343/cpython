#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

static PyObject *global_var = NULL;
volatile int terminate_thread_flag = 0;
FILE *output_fd = NULL;

static void thread_func(void *arg)
{
    PyGILState_STATE gstate;
    gstate = PyGILState_Ensure();

    // Modify the global variable in the context of this thread
    global_var = PyLong_FromLong(42);
    // PyObject_Print(global_var, stdout, 0);
    // _Py_PrintReferences(stderr);
    // FILE *output_fd = fopen("/home/lyuze/cpython/heats.txt", "w");
    // if (output_fd == NULL)
    // {
    //     perror("failed to open file\n");
    // }
    while (!terminate_thread_flag)
    {
        // _Py_PrintReferenceAddresses(output_fd);
        fprintf(output_fd, "hererr\n");
        Py_BEGIN_ALLOW_THREADS
            usleep(500000);
        // sleep(1); // This releases the GIL during sleep
        Py_END_ALLOW_THREADS
    }
    // test_thread_func(output_fd);
    // fclose(output_fd);
    PyGILState_Release(gstate);
}

static PyObject *start_thread(PyObject *self, PyObject *args)
{
    unsigned int sample_dur;
    unsigned int buff_size;
    const char *file;
    PyObject* doIO;
    if (!PyArg_ParseTuple(args, "ii|sO", &sample_dur, &buff_size, &file, &doIO))
    {
        return NULL; // error
    }
    // if (PyBool_Check(doIO)) {
    //     if (PyObject_IsTrue(doIO)) {
    //         fprintf(stderr, "do IO\n");
    //     } else {
    //         fprintf(stderr, "not do IO\n");
    //     }
    // } else {
    //     PyErr_SetString(PyExc_TypeError, "Expected a boolean object.");
    //     return NULL; 
    // }
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

    // fprintf(stderr, "buff size in shared module is %d\n", bookkeepArgs->buff_size);
    long thread_id = PyThread_start_new_thread(ref_cnt_changes, (void *)bookkeepArgs);
    if (thread_id == -1)
    {
        return NULL; // error occurred
        perror("thread spawn error\n");
    }
    Py_RETURN_NONE;
}
static PyObject *close_thread(PyObject *self, PyObject *args)
{
    // terminate_thread_flag = 1;
    terminate_flag_dummy = 1;
    // sleep(5);
    // while (terminate_flag_dummy == 1) {
    //     fprintf(stderr, "flush not complete, still waiting...\n");
    //     usleep(500000);
    // }
    // fprintf(stderr, "flush complete, closing fd\n");
    fclose(output_fd);
    Py_RETURN_NONE;
}
// static PyMethodDef ThreadMethods[] = {
//     {"start_thread", start_thread, METH_VARARGS, "Start a new thread."},
//     {NULL, NULL, 0, NULL}};
static PyMethodDef ThreadMethods[] = {
    {"start_thread", start_thread, METH_VARARGS, "Start a new thread."},
    {"close_thread", close_thread, METH_NOARGS, "Signal the thread to stop."},
    {NULL, NULL, 0, NULL}};
static struct PyModuleDef threadmodule = {
    PyModuleDef_HEAD_INIT,
    "threadmodule",
    NULL,
    -1,
    ThreadMethods};

PyMODINIT_FUNC PyInit_threadmodule(void)
{
    return PyModule_Create(&threadmodule);
}
