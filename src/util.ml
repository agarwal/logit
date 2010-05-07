(** Collection of miscellaneous functions. *)

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
