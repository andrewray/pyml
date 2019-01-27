from ctypes import *

# Build this first: 'dune build examples/shared_lb/export_to_python.so'
ocaml_shared_library = '../../_build/default/examples/shared_lib/export_to_python.so'

# Load the ocaml shared library.
ocaml = PyDLL(ocaml_shared_library, RTLD_GLOBAL)

# Initialize the OCaml runtime.
argv_t = c_char_p * 2
argv = argv_t(ocaml_shared_library.encode('utf-8'), None)
ocaml.caml_startup(argv)

# Call a function defined in OCaml using PyML.
import stuff_from_ocaml
print(stuff_from_ocaml.say_hello('andy'))
