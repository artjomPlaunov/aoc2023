open Core

type passwordPolicy = 
  {
    min : int;
    max : int; 
    ch  : char;
    password : string;
  }[@@deriving show];;

let parsePolicy (p : string) : passwordPolicy = 
  let s = String.split_on_chars ~on:[ ' ' ; '-' ; ':' ;] p in 
  let parsed = List.filter ~f:(fun x -> (String.length x) <> 0) s in
  {
    min = int_of_string (List.nth_exn parsed 0);
    max = int_of_string (List.nth_exn parsed 1);
    ch = Base.String.unsafe_get (List.nth_exn parsed 2) 0;
    password = List.nth_exn parsed 3;
  }

let explode_string s = List.init (String.length s) ~f:(String.get s);;

let checkPolicy (p : passwordPolicy) : bool =
  let chs = explode_string p.password in
  let count = List.length (List.filter ~f:(fun x -> Char.equal x p.ch) chs) in
  if count < p.min || count > p.max then false else true

  let checkNewPolicy (p : passwordPolicy) : bool =
    let chs = explode_string p.password in
    let ch1 = List.nth_exn chs (p.min-1) in
    let ch2 = List.nth_exn chs (p.max-1) in
    let b1 = Char.equal ch1 p.ch in
    let b2 = Char.equal ch2 p.ch in
    if b1 && b2 then false else if (not b1) && (not b2) then false else true         

  
