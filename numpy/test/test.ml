open! Base
open! Expect_test_helpers_kernel

let () = Py.initialize ()

let%expect_test "of_bigarray" =
  (match Py.Import.try_import_module "numpy" with
  | None -> print_s [%message "Numpy not available"]
  | Some _ -> (
    let array = [| 1.; 2. |] in
    let array1 =
      Bigarray.Array1.of_array (Bigarray.float64) (Bigarray.c_layout) array in
    let bigarray = Bigarray.genarray_of_array1 array1 in
    let a = Numpy.of_bigarray bigarray in
    let m = Py.Import.add_module "test" in
    Py.Module.set m "array" a;
    require [%here] (Py.Run.simple_string "
from test import array
assert len(array) == 2
assert array[0] == 1.
assert array[1] == 2.
array[0] = 42.
array[1] = 43.
");
    print_s [%message "" ~_:(Array.init 2 ~f:(Bigarray.Array1.get array1) : float array)]));
  [%expect {|
    "Numpy not available" |}]
;;

let%expect_test "to_bigarray" =
  (match Py.Import.try_import_module "numpy" with
  | None -> print_s [%message "Numpy not available"]
  | Some _ -> (
     let m = Py.Import.add_module "test" in
     let callback arg =
       let bigarray =
         Numpy.to_bigarray Bigarray.nativeint Bigarray.c_layout arg.(0) in
       let array1 = Bigarray.array1_of_genarray bigarray in
       print_s [%message "" ~_:(Array.init 4 ~f:(Bigarray.Array1.get array1) : Nativeint.t array)];
       Py.none in
     Py.Module.set m "callback" (Py.Callable.of_function callback);
     require [%here] (Py.Run.simple_string "
from test import callback
import numpy
callback(numpy.array([0,1,2,3]))
")));
  [%expect {| "Numpy not available" |}]
;;
