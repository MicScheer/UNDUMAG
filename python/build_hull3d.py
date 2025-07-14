
# +PATCH,//UNDUMAG/PYTHON
# +DECK,build_hull3d,T=PYTHON.

# Based on https://www.noahbrenowitz.com/post/calling-fortran-from-python/

import cffi,platform,os

ffibuilder = cffi.FFI()

header = """
extern void hull2d(void);
extern void hull3d(void);
"""

module = """
from plugin_hull3d import ffi
import numpy as np
import os,platform

if platform.system() == 'Windows':
  os.environ['PYTHONPATH'] = '..\\python'
else:
  os.environ['PYTHONPATH'] = '../python'
#endif

import hull3d as h3d
from hull3d import *

@ffi.def_extern()

def hull3d():
  hull3d_py()

@ffi.def_extern()

def hull2d():
  hull2d_py()

"""

with open("hull3d.h", "w") as f:
    f.write(header)

ffibuilder.embedding_api(header)
ffibuilder.set_source("plugin_hull3d", r'''
    #include "hull3d.h"
''')

ffibuilder.embedding_init_code(module)

if platform.system() == 'Windows':
  ffibuilder.compile(target="..\\dynlib\\libhull3d_python.dll", verbose=True)
else:
  ffibuilder.compile(target="../dynlib/libhull3d_python.so", verbose=True)
#endif
