open Printf

let dump_mcs m =
  eprintf "point: %s, ip: %s\n" (Uint32.to_string m.point) m.ip
let dump_server_info si =
  eprintf "addr: %s, memory: %d\n" si.addr si.memory


let test () =
  let c = Ketama.create_continuum_from_file "ketama.servers" in
  let v = Ketama.search_server c "abcd" in
    eprintf "total: %d\n" (Array.length c.array);
    eprintf "hash: %s\n" (Uint32.to_string (hash "abcd"));
    eprintf "found: %s\n" v.ip;
    eprintf "maxint32: %nu\n" Int32.max_int;
    Array.iter dump_mcs c.array

let () = test ()

