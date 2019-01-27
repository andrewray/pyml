(* These tests should only run on Unix systems. *)
open! Import

let%expect_test "run file with channel" =
  let result =
    Pyutils.with_temp_file "print(\"Hello, world!\")"
      (fun _file channel -> Py.Run.load (Py.Channel channel) "test.py") in
  print_s [%message (result : py_object)];
  [%expect {|
    Hello, world!
    (result None) |}]
;;

let%expect_test "interactive loop" =
  Pyutils.with_stdin_from_string "42" Py.Run.interactive ();
  let result = Py.Long.to_int (Py.last_value ()) in
  print_s[%message (result : int)];
  [%expect {|
    >>> ...
    42
    >>>
    (result 42) |}]
;;

let%expect_test "IPython" =
  require_does_not_raise [%here]
    (fun () ->
      Py.Run.frame
        (Pyutils.with_stdin_from_string "exit" (fun () ->
             match Py.Import.try_import_module "IPython" with
             | None -> print_s [%message "IPython not available"]
             | Some _ -> Py.Run.ipython ~frame:false ()))
        ());
  [%expect {| "IPython not available" |}]
;;
