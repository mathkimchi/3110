type t = { ic : in_channel; oc : out_channel }

let default_python () =
  if Sys.command "command -v uv > /dev/null 2>&1" = 0 then "uv run --quiet"
  else "python3"

let start ?python ~script () =
  if not (Sys.file_exists script) then
    failwith
      (Printf.sprintf "Torch_backend: %s not found (run from the repo root)" script);
  let python = match python with Some p -> p | None -> default_python () in
  let cmd = python ^ " " ^ Filename.quote script in
  let ic, oc = Unix.open_process cmd in
  set_binary_mode_in ic true;
  set_binary_mode_out oc true;
  match input_line ic with
  | "ready" -> { ic; oc }
  | line ->
      ignore (Unix.close_process (ic, oc));
      failwith ("Torch_backend: expected ready, got " ^ line)
  | exception End_of_file ->
      ignore (Unix.close_process (ic, oc));
      failwith
        (Printf.sprintf
           "Torch_backend: `%s` exited before it was ready; it needs Python \
            with torch (install uv, or give --python a python that has torch)"
           cmd)

let hex = Printf.sprintf "%h"

let request b line floats =
  let buf = Buffer.create (16 + (8 * Array.length floats)) in
  Printf.bprintf buf "%s floats=%d\n" line (Array.length floats);
  Array.iter (fun x -> Buffer.add_int64_le buf (Int64.bits_of_float x)) floats;
  (try
     Buffer.output_buffer b.oc buf;
     flush b.oc
   with Sys_error m -> failwith ("Torch_backend: backend died: " ^ m));
  let reply =
    try input_line b.ic with End_of_file -> failwith "Torch_backend: backend died"
  in
  match String.split_on_char ' ' reply with
  | "ok" :: count :: words ->
      let count = int_of_string count in
      let bytes = Bytes.create (8 * count) in
      really_input b.ic bytes 0 (8 * count);
      (words, Array.init count (fun i -> Int64.float_of_bits (Bytes.get_int64_le bytes (8 * i))))
  | _ -> failwith ("Torch_backend: " ^ reply)

let stop b = ignore (Unix.close_process (b.ic, b.oc))
