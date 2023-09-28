from setuptools import setup, Extension

# Define the extension module
extension_module = Extension(
    'get_gc_info',  # Name of the Python module
    sources=['test_get_gc_list.c'],
)

# Create the setup
setup(
    name='get_gc_info',
    version='1.0',
    ext_modules=[extension_module],
)
