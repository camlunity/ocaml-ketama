
open Printf

open Ketama


let dump_mcs m =
  printf "point: %s, ip: %s\n" (Uint32.to_string m.point) m.ip


let test filename =
  let c = Ketama.create_continuum_from_file filename in
  let v = Ketama.search_server c "abcd" in
    printf "total: %d\n" (Array.length c.array);
    printf "hash: %s\n" (Uint32.to_string (hash "abcd"));
    printf "found: %s\n" v.ip;
    printf "maxint32: %lu\n" Int32.max_int;
    Array.iter dump_mcs c.array


let main () =
  if Array.length Sys.argv < 2 then (
    printf "no filename supplied. using tests/ketama.servers";
    test "tests/ketama.servers"
  )
  else
    test Sys.argv.(1)


let () = main ()

