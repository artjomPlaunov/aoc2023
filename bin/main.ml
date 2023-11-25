(* read lines *)
let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

  
let () = 
  let lines = read_lines "puzzle_inputs/day2.txt" in 
  let policies = List.map Day2.parsePolicy lines in 
  let validPolicies = List.filter (Day2.checkNewPolicy) policies in
  print_endline (string_of_int (List.length validPolicies))
