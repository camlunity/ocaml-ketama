val debug : bool Batteries.ref
module Ketama :
  sig
    val ( |> ) : 'a -> ('a -> 'b) -> 'b
    val uint32_of_byte : char -> Uint32.uint32
    val dprintf :
      (int -> unit, unit BatInnerIO.output, unit) Batteries.Printf.t ->
      int -> unit
    type mcs = { point : Uint32.t; ip : string; }
    type server_info = { addr : string; memory : int; }
    type continuum = { num_points : int; array : mcs array; }
    val dump_mcs : mcs -> unit
    val dump_server_info : server_info -> unit
    val fold_file :
      ?func:(string -> string list -> string list) -> string -> string list
    val safe_of_string : string -> int
    val read_server_definitions : string -> server_info list
    val cmp_mcs : mcs -> mcs -> int
    val load_mcs : server_info list -> mcs list list
    val create_continuum : string -> mcs list
    val hash : string -> Uint32.uint32
    val search_server : continuum -> ?lowp:int -> ?highp:int -> string -> mcs
    val get_server : 'a -> continuum -> mcs
  end
val test : unit -> unit
