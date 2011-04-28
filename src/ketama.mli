
type mcs = { point : Uint32.t; ip : string; }

type server_info = { addr : string; memory : int; }

type continuum = { num_points : int; array : mcs array; }

val create_continuum : server_info array -> continuum

val create_continuum_from_file : string -> continuum

val hash : string -> Uint32.uint32

val search_server : continuum -> ?lowp:int -> ?highp:int -> string -> mcs

val get_server : continuum -> string -> mcs

