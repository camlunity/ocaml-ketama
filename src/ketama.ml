
let uint32_of_byte b = Uint32.of_int32 (Int32.of_int (Char.code b))

type node = { point: Uint32.t; ip: string }

type server_info = { addr: string; power: int }

type continuum = { num_points: int; nodes: node array }


let cmp_nodes a b =
  compare a.point b.point


let load_nodes ?(partitions=40.) servers =
  let ls = Array.length servers in
  let total_mem = Array.fold_right (fun x acc -> acc + x.power) servers 0 in
  let ftotal_mem = float_of_int total_mem in
  let lr = Uint32.logor in
  let z digest h =
    let t x h shift =
      Uint32.shift_left (uint32_of_byte digest.[x + h * 4]) shift in
    lr (lr (lr (t 3 h 24) (t 2 h 16)) (t 1 h 8)) (t 0 h 0) in

  Array.fold_right (fun srv acc ->
    let pct = (float_of_int srv.power) /. ftotal_mem in
    let ks = int_of_float (floor (pct *. partitions *. (float_of_int ls))) in
    Array.append acc
      (Array.init (ks * 4) (fun i ->
        let ss = Printf.sprintf "%s-%d" srv.addr (i / 4) in
        let digest = Digest.string ss in
        let z' = z digest in
        {point=z' (i mod 4); ip = srv.addr}
       ))
  ) servers [||]

let create_continuum ?(partitions=40.) servers  =
  let nodes = load_nodes ~partitions:partitions servers in
  let () = Array.sort cmp_nodes nodes in
  let num = Array.length nodes in
  if num == 0
  then failwith "No elements in continuum!"
  else {num_points=num; nodes=nodes}


let hash str =
  let s = Digest.string str in
  let lr = Uint32.logor in
  let t x y = Uint32.shift_left (uint32_of_byte s.[x]) y in
    lr (lr (lr (t 3 24) (t 2 16)) (t 1 8)) (t 0 0)


let cycle_sub arr start len =
  let length = Array.length arr in
  if (start + len <= length ) then Array.sub arr start len
  else (
    let sb = length - start in
    let rest = len - sb in
    let fst = Array.sub arr start sb in
    let snd = Array.sub arr 0 rest in
    Array.append fst snd
  )


let get_n_servers c n k =
  let lowp = 0 in
  let highp = c.num_points in
  let h = hash k in
  let a = c.nodes in
  let rec search l u =
    let m = (l + u) / 2 in
    if m = c.num_points
    then cycle_sub a 0 n
    else (
      let mv = a.(m).point in
      let mv1 = if m=0 then Uint32.zero else a.(m - 1).point in
      match ( mv1 < h, h <= mv) with
        | (true, true) -> cycle_sub a m n (* between *)
        | (true, false)  -> search (m + 1) u (* before m - 1 *)
        | (false, true) -> search l (m - 1) (* after m *)
        | (false, false) -> cycle_sub a 0 n
    )
  in search lowp highp


let get_server c k =
  (get_n_servers c 1 k).(0)


let safe_of_string s =
  try
    int_of_string s
  with Failure _ -> failwith (Printf.sprintf "Invalid power value: %s" s)


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
      | addr::power::[] -> {addr=addr; power=safe_of_string power }
      | _ -> failwith (Printf.sprintf "Invalid config string: %s" x)
  in
  let filt s = String.length s = 0 || s.[0] != '#'
  in
  try
    let lines = fold_file filename in
    List.map mapper (List.filter filt lines)
  with Sys_error _ ->
    failwith (Printf.sprintf "Unable to open file %s" filename)


let create_continuum_from_file ?(partitions=40.0) filename =
  let servers = Array.of_list (read_server_definitions filename) in
  create_continuum ~partitions:partitions servers

