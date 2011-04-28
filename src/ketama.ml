
open Batteries
open Digest
open Printf
open Int

(*
  Note: use slightly updated version of ocaml-uint library from
  http://github.com/gene9/ocaml-uint.
*)

let debug = ref true

module Ketama =
  struct

let (|>) x f = f x
let uint32_of_byte b = Char.code b |> Int32.of_int |> Uint32.of_int32

let dprintf =
  fprintf (if !debug then IO.stderr else IO.stdnull)

type mcs = { point: Uint32.t; ip: string }
type server_info = { addr: string; memory: int }
type continuum = { num_points: int; array: mcs array }

let dump_mcs m =
  eprintf "point: %s, ip: %s\n" (Uint32.to_string m.point) m.ip
let dump_server_info si =
  eprintf "addr: %s, memory: %d\n" si.addr si.memory

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

let safe_of_string s =
  try
    of_string s
  with Failure _ -> failwith (sprintf "Invalid memory value: %s" s)

let read_server_definitions filename =
  let re = Str.regexp "[ \t]" in
  let mapper x =
    let z = Str.split re x in
      { addr = List.at z 0; memory = safe_of_string (List.at z 1) } in
  let filt s = not (String.starts_with s "#") in
    try
      let lines = fold_file filename in
        List.map mapper (List.filter filt lines)
    with Sys_error _ ->
      failwith (sprintf "Unable to open file %s" filename)

let cmp_mcs a b =
  if Uint32.( < ) a.point b.point then 1 else
    if Uint32.( > ) a.point b.point then -1
    else 0

let load_mcs servers =
  let ls = List.length servers in
  let total_mem = List.fold_right (fun x acc -> acc + x.memory) servers 0 in
  let ftotal_mem = Float.of_int total_mem in
  let create_mcs srv =
    let pct = (Float.of_int srv.memory) /. ftotal_mem in
    let ks = Int.of_float (Float.floor (pct *. 40.0 *. (Float.of_int ls))) in
    let rc =
      List.mapi (fun k x ->
                   let ss = sprintf "%s-%d" srv.addr k in
                   let digest = Digest.string ss in
		   let lr = Uint32.logor in 
		   let t x h shift =
                     Uint32.shift_left (uint32_of_byte digest.[x + h * 4]) shift in
                   let z h =
		     lr (lr (lr (t 3 h 24) (t 2 h 16)) (t 1 h 8)) (t 0 h 0)
                   and mc = List.make 4 { point = Uint32.zero; ip = srv.addr } in
                     List.mapi (fun h x -> {x with point = z h}) mc        
                ) (List.of_enum (0--^ks))
    in List.flatten rc
  in List.map create_mcs servers

let create_continuum filename =
  let servers = List.rev (read_server_definitions filename) in
  let c = List.sort ~cmp:cmp_mcs (List.flatten (load_mcs servers)) in c

let hash str =
  let s = Digest.string str in
  let lr = Uint32.logor in
  let t x y = Uint32.shift_left (uint32_of_byte s.[x]) y in
    lr (lr (lr (t 3 24) (t 2 16)) (t 1 8)) (t 0 0)

let rec search_server c ?(lowp=0) ?(highp=c.num_points) k =
  let h = hash k
  and a = c.array in
  let rec search' l u =
    let m = (l + u) / 2 in
      eprintf "m: %d\n" m;
      if m == c.num_points then a.(0)
      else (
        let mv = a.(m).point in
        let mv1 = match m with 0 -> Uint32.zero | _ -> a.(m - 1).point in
          (* eprintf "mv: %s mv1: %s\n" (Uint32.to_string mv) (Uint32.to_string mv1); *)
          match (Uint32.( <= ) h mv, Uint32.( > ) h mv1) with
              (true, true) -> a.(m)
	    | _ -> (
	        let param = 
		  if Uint32.( < ) mv h then (m + 1, u) else (l, m - 1) in
		  if (fst param) > (snd param) then a.(0) 
		  else search' (fst param) (snd param)
	      )
      )
  in search' lowp highp
        
let get_server key c =
  let highp = c.num_points in
    search_server c ~lowp:0 ~highp:highp ""
end

open Ketama

let test() = 
  let mcss = Ketama.create_continuum "ketama.servers" in
  let c = { num_points = List.length mcss; array = Array.of_list mcss } in
  let v = Ketama.search_server c "abcd" in
    dprintf "total: %d\n" (Array.length c.array);
    eprintf "hash: %s\n" (Uint32.to_string (hash "abcd"));
    eprintf "found: %s\n" v.ip;
    eprintf "maxint32: %nu\n" Nativeint.max_int;
    Array.iter dump_mcs c.array

let _ = test()

(* *)
