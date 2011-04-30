
type node = private { point : Uint32.t; ip : string; }

type server_info = { addr : string; power : int; }

type continuum = private { num_points : int; nodes : node array; }

val create_continuum : ?partitions:float -> server_info array -> continuum

val create_continuum_from_file : ?partitions:float -> string -> continuum

val hash : string -> Uint32.uint32

val search_server : continuum -> ?lowp:int -> ?highp:int -> string -> node

val get_server : continuum -> string -> node

