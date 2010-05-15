(** Miscellaneous functions. *)

open Printf

(** Convert string to char list. *)
let explode (s:string) : char list =
  let ans = ref [] in
  for i = 0 to String.length s - 1 do
    ans := s.[i]::!ans
  done;
  List.rev !ans
    
(** Convert char list to string. *)
let implode (cl : char list) : string =
  let s = String.create (List.length cl) in
  let rec loop i cl = match cl with
    | [] -> s
    | c::cl -> (s.[i] <- c; loop (i+1) cl)
  in
  loop 0 cl

(** [string_drop n s] returns a fresh string where the first [n]
    characters in [s] have been dropped. Raise [Failure] if n < 0 or
    greater than string length. *)
let string_drop (n:int) (s:string) : string =
  let l = String.length s in
  if n < 0 then
    failwith (sprintf "string_drop: cannot drop %d characters" n)
  else if n > l then
    failwith (sprintf "string_drop: cannot drop %d characters from string of length %d" n l)
  else
    String.sub s n (l-n)

(** [string_take n s] returns a fresh string where only the first [n]
    characters in [s] have been kept. Raise [Failure] if n < 0 or
    greater than string length. *)
let string_take (n:int) (s:string) : string =
  let l = String.length s in
  if n < 0 then
    failwith (sprintf "string_take: cannot take %d characters" n)
  else if n > l then
    failwith (sprintf "string_take: cannot take %d characters from string of length %d" n l)
  else
    String.sub s 0 n
  
