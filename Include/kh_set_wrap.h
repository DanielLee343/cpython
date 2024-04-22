#include <Python.h>
#include "khash.h"

void init_global_set_helper();
void destroy_global_set_helper();
void insert_global_set_helper(PyObject *op);
int found_in_kset_helper(PyObject *op);
void erase_item_global_set_helper(PyObject *op);
unsigned int get_global_kh_size();
