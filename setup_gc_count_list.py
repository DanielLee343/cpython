from setuptools import setup, Extension

# Define the extension module
extension_module = Extension(
    'gc_count_module',  # Name of the Python module
    sources=['thread_get_gc_count.c'],
)

# Create the setup
setup(
    name='gc_count_module',
    version='1.0',
    ext_modules=[extension_module],
)
