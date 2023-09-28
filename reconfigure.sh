/home/lyuze/workspace/cpython/python -m pip uninstall -y setuptools Cython cython
/home/lyuze/workspace/cpython/python -m pip install setuptools
cd ~/workspace/cython
/home/lyuze/workspace/cpython/python -m pip install .
cd ~/workspace/py_track
rm -rf build
# /home/lyuze/workspace/cpython/python setup.py build_ext --inplace
/home/lyuze/workspace/cpython/python setup_gc_count_list.py build_ext --inplace