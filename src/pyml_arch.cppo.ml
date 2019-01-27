#ifdef cygwin

let library_patterns: (int -> int -> string, unit, string) format list =
  ["python%d%d.dll"]

let library_suffix = ".dll"

let ensure_executable_suffix executable =
  if Filename.check_suffix executable ".exe" then
    executable
  else
    executable ^ ".exe"

let which = "where"

external fd_of_int: int -> Unix.file_descr = "win_handle_fd"

let path_separator = ";"

#end

#elif defined macosx

let library_patterns: (int -> int -> string, unit, string) format list =
  ["libpython%d.%dm.dylib"; "libpython%d.%d.dylib"]

let library_suffix = ".dylib"

let ensure_executable_suffix executable = executable

let which = "which"

external fd_of_int: int -> Unix.file_descr = "%identity"

let path_separator = ":"

#elif defined linux

let library_patterns: (int -> int -> string, unit, string) format list =
  ["libpython%d.%dm.so"; "libpython%d.%d.so"]

let library_suffix = ".so"

let ensure_executable_suffix executable = executable

let which = "which"

external fd_of_int: int -> Unix.file_descr = "%identity"

let path_separator = ":"

#else

(* force a build error*)
Unsupported_platform_in_pyml_arch

#endif
