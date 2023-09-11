#include <Python.h>
static PyObject* python_object_size(PyObject* self, PyObject* args) {
    PyObject* myObject = PyList_New(10);
    Py_ssize_t size = PyObject_Size(myObject);
    fprintf(stderr, "Size of the object: %zd bytes\n", size);

    Py_RETURN_NONE;
}

static PyMethodDef my_extension_methods[] = {
    {"python_object_size", python_object_size, METH_NOARGS, "Description"},
    {NULL, NULL, 0, NULL}
};

static struct PyModuleDef python_object_size_module = {
    PyModuleDef_HEAD_INIT,
    "python_object_size",  // Module name
    NULL,            // Module documentation
    -1,              // Size of per-interpreter state of the module
    my_extension_methods
};

PyMODINIT_FUNC PyInit_python_object_size(void) {
    return PyModule_Create(&python_object_size_module);
}
