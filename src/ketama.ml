
(*
  Note: use slightly updated version of ocaml-uint library from
  http://github.com/gene9/ocaml-uint.
*)


let uint32_of_byte b = Uint32.of_int32 (Int32.of_int (Char.code b))

type mcs = { point: Uint32.t; ip: string }

type server_info = { addr: string; memory: int }

type continuum = { num_points: int; array: mcs array }


let cmp_mcs a b =
  if Uint32.( < ) a.point b.point then 1
  else
    if Uint32.( > ) a.point b.point
    then -1
    else 0


let load_mcs servers =
  let ls = Array.length servers in
  let total_mem = Array.fold_right (fun x acc -> acc + x.memory) servers 0 in
  let ftotal_mem = float_of_int total_mem in
  let lr = Uint32.logor in
  Array.fold_right (fun srv acc ->
    let pct = (float_of_int srv.memory) /. ftotal_mem in
    let ks = int_of_float (floor (pct *. 40.0 *. (float_of_int ls))) in
    Array.append acc
      (Array.init (ks * 4) (fun i ->
        let ss = Printf.sprintf "%s-%d" srv.addr (i mod ks) in
        let digest = Digest.string ss in
        let t x h shift =
          Uint32.shift_left (uint32_of_byte digest.[x + h * 4]) shift in
        let z h =
          lr (lr (lr (t 3 h 24) (t 2 h 16)) (t 1 h 8)) (t 0 h 0) in
        {point=z (i mod 4); ip = srv.addr}
       ))
  ) servers [||]


let create_continuum servers =
  let mcs = load_mcs servers in
  let () = Array.sort cmp_mcs mcs in
  let num = Array.length mcs in
  if num == 0
  then failwith "No elements in continuum!"
  else {num_points=num; array=mcs}


let hash str =
  let s = Digest.string str in
  let lr = Uint32.logor in
  let t x y = Uint32.shift_left (uint32_of_byte s.[x]) y in
    lr (lr (lr (t 3 24) (t 2 16)) (t 1 8)) (t 0 0)

let search_server c ?(lowp=0) ?(highp=c.num_points) k =
  let h = hash k
  and a = c.array in
  let rec search l u =
    let m = (l + u) / 2 in
    if m == c.num_points
    then a.(0)
    else (
      let mv = a.(m).point in
      let mv1 = a.(m - 1).point in
      match (Uint32.( <= ) h mv, Uint32.( > ) h mv1) with
        | (true, true) -> a.(m) (* between *)
        | (false, true)  -> search (m + 1) u (* before m - 1 *)
        | (false, false) -> search l (m - 1) (* after m *)
        | (true, false) -> a.(0)
    )
  in search lowp highp

let get_server c key =
  search_server c key

let safe_of_string s =
  try
    int_of_string s
  with Failure _ -> failwith (Printf.sprintf "Invalid memory value: %s" s)


let fold_file ?(func = fun x y -> x::y) filename =
  let acc = ref [] in
  let chan = open_in filename in
    begin
      try while true do
        let d = input_line chan in
          acc := func d !acc
      done
      with End_of_file -> close_in chan; ()
    end;
    !acc


let read_server_definitions filename =
  let re = Str.regexp "[ \t]" in
  let mapper x =
    match Str.split re x with
      | addr::memory::[] -> {addr=addr; memory=safe_of_string memory }
      | _ -> failwith (Printf.sprintf "Invalid config string: %s" x)
  in
  let filt s = String.length s = 0 || s.[0] != '#'
  in
  try
    let lines = fold_file filename in
    List.map mapper (List.filter filt lines)
  with Sys_error _ ->
    failwith (Printf.sprintf "Unable to open file %s" filename)


let create_continuum_from_file filename =
  let servers = Array.of_list (read_server_definitions filename) in
  create_continuum servers

