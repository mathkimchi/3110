type t = { seed : int; hider : Ppo.agent option; hunter : Ppo.agent option }

let save file w =
  let oc = open_out file in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () ->
      Printf.fprintf oc "seed %d\n" w.seed;
      let side name = function
        | Some ag ->
            Printf.fprintf oc "%s\n" name;
            Ppo.write oc ag
        | None -> ()
      in
      side "hider" w.hider;
      side "hunter" w.hunter;
      output_string oc "end\n")

let bad file why =
  failwith (Printf.sprintf "Weights.load: %s is not a weights file (%s)" file why)

let load file =
  let ic = Scanf.Scanning.open_in file in
  Fun.protect
    ~finally:(fun () -> Scanf.Scanning.close_in ic)
    (fun () ->
      try
        let seed = Scanf.bscanf ic " seed %d" Fun.id in
        let rec go w =
          match Scanf.bscanf ic " %s" Fun.id with
          | "hider" -> go { w with hider = Some (Ppo.read ic) }
          | "hunter" -> go { w with hunter = Some (Ppo.read ic) }
          | "end" -> w
          | s -> failwith ("unexpected " ^ s)
        in
        go { seed; hider = None; hunter = None }
      with
      | Scanf.Scan_failure m | Failure m -> bad file m
      | End_of_file -> bad file "file ends early")
