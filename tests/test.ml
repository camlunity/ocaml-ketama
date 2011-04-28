
open QuickCheck

open Ketama

let testable_string_to_bool =
  testable_fun arbitrary_string show_string testable_bool

let cls = quickCheck testable_string_to_bool

let test filename =
  let c_many = create_continuum_from_file filename in
  let c_one = create_continuum [| {addr="127.0.0.1"; memory=100} |] in
  let prop_one_for_one s =
    let mcs = get_server c_one s in
    mcs.ip = "127.0.0.1"
  in
  let prop_ident_for_equal s =
    let mcs1 = get_server c_many s in
    let mcs2 = get_server c_many s in
    mcs1 == mcs2
  in
  let () = cls prop_one_for_one in
  let () = cls prop_ident_for_equal in
  ()


let main () =
  if Array.length Sys.argv < 2 then (
    Printf.printf "no filename supplied. using tests/ketama.servers\n";
    test "tests/ketama.servers"
  )
  else
    test Sys.argv.(1)


let () = main ()

