#include <Python.h>

static PyObject *get_gc_list(PyObject *self, PyObject *args) {
    // ... (some code)
    Py_ssize_t gen = 0;
    PyObject *pylist_obj = gc_get_objects_impl_no_mod(gen);
    if (!pylist_obj) {
        return NULL;
    }
    // PyObject_Print(pylist_obj, stderr, 1);
    // if (!PyList_Check(pylist_obj)) {
    //     PyErr_SetString(PyExc_TypeError, "Object is not a list");
    //     return NULL; // or handle error appropriately
    // }

    // Set the elements of the list
    // for (int i = 0; i < 3; i++) {
    //     PyObject *pInt = PyLong_FromLong(i);
    //     if (!pInt) {
    //         Py_DECREF(pList);
    //         return NULL;
    //     }
    //     // Note: PyList_SetItem steals a reference to the item, so no need to DECREF pInt
    //     PyList_SetItem(pList, i, pInt);
    // }
    // Check and handle any pending signals
    if (PyErr_CheckSignals() == -1) {
        // An exception (like a keyboard interrupt) occurred while checking signals
        return NULL;  // Return NULL to propagate the exception to Python
    }

    // ... (more code)

    Py_RETURN_NONE;
}

static PyMethodDef MyMethods[] = {
    {"get_gc_list",  get_gc_list, METH_NOARGS, "A function that gets gc list"},
    {NULL, NULL, 0, NULL}        /* Sentinel */
};

static struct PyModuleDef mymodule = {
    PyModuleDef_HEAD_INIT,
    "get_gc_info",   /* name of module */
    NULL,         /* module documentation, may be NULL */
    -1,           /* size of per-interpreter state of the module,
                     or -1 if the module keeps state in global variables. */
    MyMethods
};

PyMODINIT_FUNC PyInit_get_gc_info(void) {
    return PyModule_Create(&mymodule);
}

// #include <Python.h>
// PyObject* create_pylist(void) {
//     // Create a new Python list of size 3
//     PyObject *pList = PyList_New(3);

//     if (!pList) {
//         return NULL;
//     }

//     // Set the elements of the list
//     for (int i = 0; i < 3; i++) {
//         PyObject *pInt = PyLong_FromLong(i);
//         if (!pInt) {
//             Py_DECREF(pList);
//             return NULL;
//         }
//         // Note: PyList_SetItem steals a reference to the item, so no need to DECREF pInt
//         PyList_SetItem(pList, i, pInt);
//     }

//     return pList;
// }
// int main(int argc, char* argv[]) {
//     Py_Initialize();

//     PyObject *pList = create_pylist();
//     if (pList) {
//         PyObject_Print(pList, stdout, 0);
//         Py_DECREF(pList);
//     }

//     Py_Finalize();
//     return 0;
// }
