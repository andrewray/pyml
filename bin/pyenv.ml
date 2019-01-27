(* Utility to print python environment information.

   $ dune build bin/pyenv.exe
   % ./_build/default/bin/pyenv.exe
 *)

let show_environment_variable envvar =
  try Printf.eprintf "%s=%s\n" envvar (Sys.getenv envvar)
  with Not_found -> Printf.eprintf "%s not set\n" envvar

let () =
  Printf.eprintf "Pyml environment\n\n";
  Printf.eprintf "Environment variables ...\n\n";
  show_environment_variable "PATH";
  show_environment_variable "PYTHONHOME";
  show_environment_variable "DYLD_LIBRARY_PATH";
  show_environment_variable "DYLD_FALLBACK_LIBRARY_PATH";
  Printf.eprintf "\nInitialize library ...\n\n";
  Py.initialize ~verbose:true ();
  Printf.eprintf "\nDebug build ...\n\n";
  if Py.is_debug_build ()
  then Printf.eprintf "Yes"
  else Printf.eprintf "No";
  Printf.eprintf "\n\nPython version ...\n\n";
  Printf.eprintf "Version is %s\n" (Py.version ());
