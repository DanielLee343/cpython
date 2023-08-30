from setuptools import setup, Extension

module = Extension('threadmodule', sources=['threadmodule.c'])

setup(name='ThreadModule',
      version='1.0',
      description='This is a demo module for threading in CPython',
      ext_modules=[module])
