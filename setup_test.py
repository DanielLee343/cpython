from setuptools import setup, Extension

# Define the extension module
extension_module = Extension(
    'python_object_size',  # Name of the Python module
    sources=['python_object_size.c'],  # C source file
)

# Create the setup
setup(
    name='python_object_size',
    version='1.0',
    ext_modules=[extension_module],
)
