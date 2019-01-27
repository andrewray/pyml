include Base
include Expect_test_helpers_kernel

let sexp_of_py_object (x : Py.Object.t) =
  sexp_of_string (Py.Object.to_string x)

let () = 
  if not (Py.is_initialized ())
  then Py.initialize ()
