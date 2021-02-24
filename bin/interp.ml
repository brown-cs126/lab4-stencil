open Core
open Csci1260

let command =
  Command.basic ~summary:"Interpret the given file"
    Command.Let_syntax.(
      let%map_open filename = anon ("filename" %: Filename.arg_type) in
      fun () ->
        try S_exp.parse_file filename |> Interp.interp |> printf "%s\n"
        with e -> Printf.eprintf "Error: %s\n" (Exn.to_string e))

let () = Command.run ~version:"1.0" command
