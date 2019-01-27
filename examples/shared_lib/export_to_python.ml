let () = if not (Py.is_initialized ()) then Py.initialize ()

let () =
  let modl = Py.Import.add_module "stuff_from_ocaml" in
  Py.Module.set_function modl "say_hello" (fun args ->
      Py.String.to_string args.(0) |> ( ^ ) "Hello " |> Py.String.of_string )
