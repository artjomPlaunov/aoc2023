let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

let wordToDigit w = 
  match w with 
  | "zero" -> "0"
  | "one" -> "1"
  | "two" -> "2"
  | "three" -> "3"
  | "four" -> "4"
  | "five" -> "5"
  | "six" -> "6"
  | "seven" -> "7"
  | "eight" -> "8"
  | "nine" -> "9"
  | _ -> w
(* part 1
let firstLast x = 
  String.concat ""
    [(Core.Char.to_string (List.hd x));
    (Core.Char.to_string (List.hd (List.rev x)))]
*)

let day1 = 
  let lines = read_lines "puzzle_inputs/day1.txt" in
  
  (*part 1 
  let chrLsts =     List.map explode_string lines in
  let digits =      List.map (List.filter (fun x -> Core.Char.is_digit x)) chrLsts in
  let stringVals =  List.map firstLast digits in
  let vals =        List.map int_of_string stringVals in
  let res =         List.fold_right (fun x acc -> x + acc) vals 0 in
  print_endline (string_of_int res)
  *)
  let r = Str.regexp 
  {|one\|two\|three\|four\|five\|six\|seven\|eight\|nine\|1\|2\|3\|4\|5\|6\|7\|8\|9|} in
  let firstNums = 
      List.map (fun s -> 
                  let _ = Str.search_forward r s 0 in 
                  Str.matched_string s) lines in
  let lastNums = 
      List.map (fun s -> 
                  let _ = Str.search_backward r s ((String.length s)-1) in 
                  Str.matched_string s) lines in
  let firstNums = List.map wordToDigit firstNums in
  let lastNums = List.map wordToDigit lastNums in
  let nums = List.map (fun (x,y) -> int_of_string (String.concat "" [x;y])) (List.combine firstNums lastNums) in
  let res = List.fold_right (fun x acc -> x + acc) nums 0 in    
  print_endline (string_of_int res)