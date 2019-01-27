open! Import

let%expect_test "Can interrogate versions" =
  (* Don't print the versions as the output will depend on where the test is run. *)
  require_does_not_raise [%here] (fun () -> Py.version () |> ignore);
  [%expect {| |}];
  require_does_not_raise [%here] (fun () -> Py.get_version () |> ignore);
  [%expect {| |}];
;;

let%expect_test "put python output into unbuffered mode" = 
  Py.Run.eval ~start:Py.File {|
import sys,os
sys.stdout = os.fdopen(sys.stdout.fileno(), 'w', 0)
   |} |> ignore;
  [%expect {| |}]
;;

let%expect_test "hello world" =
  require [%here] (Py.Run.simple_string "print('Hello world!')");
  [%expect {| Hello world! |}]
;;

let%expect_test "class" =
  let m = Py.Import.add_module "test" in
  let value_obtained = ref None in
  let callback arg =
    value_obtained := Some (Py.String.to_string (Py.Tuple.get_item arg 1));
    Py.none in
  let c =
    Py.Class.init "myClass"
      ~methods:[("callback", Py.Callable.of_function_as_tuple callback)] in
  Py.Module.set m "myClass" c;
  require [%here] (Py.Run.simple_string "
from test import myClass
myClass().callback('OK')
");
  print_s [%message (!value_obtained : string option)];
  [%expect {| (!value_obtained (OK)) |}]
;;

let%expect_test "empty tuple" =
  require [%here] (Poly.equal (Py.Tuple.create 0) Py.Tuple.empty);
  [%expect {| |}]
;;

let%expect_test "make tuple" =
  require [%here] (Poly.equal
                     (Py.Tuple.to_singleton (Py.Tuple.singleton (Py.Long.of_int 0)))
                     (Py.Long.of_int 0));
  [%expect {| |}]
;;

let%expect_test "module get/set/remove" =
  let m = Py.Module.create "test" in
  Py.Module.set m "test" Py.none;
  require [%here] (Poly.equal (Py.Module.get m "test") Py.none);
  [%expect {| |}];
  Py.Module.remove m "test";
  require_does_raise [%here] (fun () -> ignore (Py.Module.get m "test"));
  [%expect {| ("E (<type 'exceptions.AttributeError'>, 'module' object has no attribute 'test')") |}];
;;

let%expect_test "capsule" =
  let (wrap, unwrap) = Py.Capsule.make "string" in
  let m = Py.Import.add_module "test" in
  let pywrap args =
    let s = Py.String.to_string args.(0) in
    wrap s in
  let pyunwrap args =
    let s = unwrap args.(0) in
    Py.String.of_string s in
  Py.Module.set_function m "wrap" pywrap;
  Py.Module.set_function m "unwrap" pyunwrap;
  require [%here] (Py.Run.simple_string {|
from test import wrap, unwrap
import re
x = wrap('OK')
print('Capsule type: {0}'.format(re.findall('".*"', str(x))))
print(unwrap(x))
|});
  [%expect {|
    Capsule type: ['"ocaml-capsule"']
    OK |}];
;;

let%expect_test "exception w" =
  require_does_raise [%here] (fun () ->
  ignore (Py.Run.eval ~start:Py.File "
raise Exception('Great')
"));
  [%expect {| ("E (<type 'exceptions.Exception'>, Great)") |}]
;;

let%expect_test "ocaml exception" =
  let m = Py.Import.add_module "test" in
  let mywrap _ =
    raise (Py.Err (Py.Err.Exception, "Great")) in
  Py.Module.set_function m "mywrap" mywrap;
  require [%here] (Py.Run.simple_string {|
from test import mywrap
try:
    mywrap()
    raise Exception('No exception raised')
except Exception as err:
    assert str(err) == "Great"
|});
  [%expect {| |}]
;;

let%expect_test "ocaml other exception" =
  let m = Py.Import.add_module "test" in
  let mywrap _ = raise Caml.Exit in
  Py.Module.set_function m "mywrap" mywrap;
  require_does_raise [%here] (fun () ->
      require [%here] (Py.Run.simple_string "
from test import mywrap
try:
    mywrap()
except Exception as err:
    raise Exception('Should not be caught by Python')
"));
  [%expect {| Exit |}]
;;

let%expect_test "run file with filename" =
  let result =
    Pyutils.with_temp_file "print(\"Hello, world!\")"
      (fun file _channel -> Py.Run.load (Py.Filename file) "test.py") in
  print_s [%message (result : py_object)];
  [%expect {|
    Hello, world!
    (result None) |}]
;;

let%expect_test "boolean" =
  require [%here] (Py.Bool.to_bool (Py.Run.eval "True"));
  require [%here] (not (Py.Bool.to_bool (Py.Run.eval "False")));
  [%expect {||}]
;;

let%expect_test "reinitialize" =
  Py.finalize ();
  require_does_raise [%here] (fun () ->
      (Py.Run.simple_string "not initialized"));
  (* let (version, minor) = !Pyml_tests_common.use_version in *)
  (* CR aray: select version... *)
  require_does_not_raise [%here] (fun () -> Py.initialize ());
  [%expect {|
    (Failure "Run 'Py.initialize ()' first") |}]
;;

let%expect_test "string conversion error" = 
  require_does_raise [%here] (fun () -> Py.String.to_string (Py.Long.of_int 0));
  [%expect {| (Failure "Type mismatch: String or Unicode expected. Got: Long (0L)") |}]
;;

let%expect_test "float conversion error" =
  require_does_raise [%here] (fun () -> Py.Float.to_float (Py.String.of_string "a"));
  [%expect {| ("E (<type 'exceptions.TypeError'>, a float is required)") |}]
;;

let%expect_test "long conversion error" =
  require_does_raise [%here] (fun () -> Py.Long.to_int (Py.String.of_string "a"));
  [%expect {| ("E (<type 'exceptions.TypeError'>, an integer is required)") |}]
;;

let%expect_test "iterators" =
  let iter = Py.Object.get_iter (Py.Run.eval "['a','b','c']") in
  let list = Py.Iter.to_list_map Py.String.to_string iter in
  print_s [%message (list : string list)];
  [%expect {| (list (a b c)) |}]
;;

let%expect_test "Dict.iter" =
  let dict = Py.Dict.create () in
  for i = 0 to 9 do
    Py.Dict.set_item_string dict (Int.to_string i) (Py.Long.of_int i)
  done;
  let table = Array.create ~len:10 None in
  Py.Dict.iter begin fun key value ->
    let index = Py.Long.to_int value in
    require [%here] (Poly.equal table.(index) None);
    table.(index) <- Some (Py.String.to_string key)
    end dict;
  print_s [%message (table : string option array)];
  [%expect {|
    (table (
      (0)
      (1)
      (2)
      (3)
      (4)
      (5)
      (6)
      (7)
      (8)
      (9))) |}]
;;

let%expect_test "unicode" =
  let codepoints = [| 8203; 127; 83; 2384; 0; 12 |] in
  let python_string = Py.String.of_unicode codepoints in
  let ocaml_string = Py.String.to_string python_string in
  let python_string' = Py.String.decode_UTF8 ocaml_string in
  let codepoints' = Py.String.to_unicode python_string' in
  print_s [%message (codepoints : int array) (codepoints' : int array)];
  [%expect {| ((codepoints (8203 127 83 2384 0 12)) (codepoints' (8203 127 83 2384 0 12))) |}]
;;

let%expect_test "Marshal" =
  let v = Py.Long.of_int 42 in
  let m = Py.Marshal.dumps v in
  let v' = Py.Marshal.loads m in
  print_s [%message "" ~_:(Py.Long.to_int v' : int)];
  [%expect {| 42 |}]
;;

let%expect_test "Py.List.of_list" =
  let v = Py.List.of_list [Py.Long.of_int 42] in
  print_s [%message "" ~length:(Py.List.length v : int)];
  print_s [%message "" ~value:(Py.Long.to_int (Py.List.get v 0) : int)];
  [%expect {|
    (length 1)
    (value 42) |}]
;;

let%expect_test "Py.List.sort" =
  let pi_digits = [ 3; 1; 4; 1; 5; 9; 2; 6; 5; 3; 5; 8 ] in
  let v = Py.List.of_list [] in
  require [%here] (Py.List.length v = 0);
  let count = Py.Object.call_method v "count" [|Py.Long.of_int 1|] in
  require [%here] (Py.Long.to_int count = 0);
  List.iter
    pi_digits
    ~f:(fun i -> ignore (Py.Object.call_method v "append" [|Py.Long.of_int i|]));
  let count = Py.Object.call_method v "count" [|Py.Long.of_int 1|] in
  require [%here] (Py.Long.to_int count = 2);
  require [%here] (Py.List.length v = List.length pi_digits);
  let _ = Py.Object.call_method v "sort" [||] in
  let sorted_digits = List.map (Py.List.to_list v) ~f:Py.Int.to_int in
  print_s [%message "sorted digits"
              (sorted_digits : int list)
              ~expected:(List.sort ~compare pi_digits : int list)];
  if Py.version_major () >= 3
  then (
    let _ = Py.Object.call_method v "clear" [||] in
    require [%here] (Py.List.length v = 0));
  [%expect {|
    ("sorted digits"
      (sorted_digits (1 1 2 3 3 4 5 5 5 6 8 9))
      (expected (1 1 2 3 3 4 5 5 5 6 8 9))) |}]
;;

let%expect_test "array" =
  let array = [| 1; 2 |] in
  let a = Py.Array.of_array Py.Long.of_int Py.Long.to_int array in
  let m = Py.Import.add_module "test" in
  Py.Module.set m "array" a;
  require [%here] (Py.Run.simple_string "
from test import array
assert len(array) == 2
assert array[0] == 1
assert array[1] == 2
array[0] = 42
array[1] = 43
copy = []
for x in array:
  copy.append(x)
assert copy == [42, 43]
");
  print_s [%message (array : int array)];
  [%expect {| (array (42 43)) |}]
;;

let%expect_test "numpy" =
  (match Py.Import.try_import_module "numpy" with
  | None -> print_s [%message "Numpy not available"]
  | Some _ -> (
    print_s [%message "execute"];
    let array = Stdcompat.Array.Floatarray.create 2 in
    Stdcompat.Array.Floatarray.set array 0 1.;
    Stdcompat.Array.Floatarray.set array 1 2.;
    let a = Py.Array.numpy array in
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
    let to_float_array a =
      Array.init (Stdcompat.Array.Floatarray.length a)
        ~f:(Stdcompat.Array.Floatarray.get a) in
    print_s [%message "" ~_:(to_float_array array : float array)]));
  [%expect {| "Numpy not available" |}]
;;

