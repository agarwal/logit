(** Format strings, used to represent both how input file names are
    parsed and how output file names are printed. *)

open Printf

type num_field = Year | Month | Day | Hour | Minute | Second
type pm = Plus | Minus

(** Each format element represents how to parse/print a specific type of value. *)    
type elem = 
    | Char of char (** a specific single character *)
    | String (** sequence of any characters until subsequent [elem] in [t] succeeds *)
    | DefaultNum of num_field
    | ExactNum of int * num_field
    | DeltaNum of pm * int * num_field
        
(** A format string is an ordered list of format elements. *)
type t = elem list

let explode (s:string) : char list =
  let ans = ref [] in
  for i = 0 to String.length s - 1 do
    ans := s.[i]::!ans
  done;
  List.rev !ans
    
let implode (cl : char list) : string =
  let s = String.create (List.length cl) in
  let rec loop i cl = match cl with
    | [] -> s
    | c::cl -> (s.[i] <- c; loop (i+1) cl)
  in
  loop 0 cl
    
(** Return [elem] for given [c] assuming [c] occurred immediately
    after a '%' symbol. *)
let parse_percent_char (c:char) : elem =
  match c with
    | 's' -> String
    | 'y' -> DefaultNum Year
    | 'm' -> DefaultNum Month
    | 'd' -> DefaultNum Day
    | 'h' -> DefaultNum Hour
    | 'n' -> DefaultNum Minute
    | 'c' -> DefaultNum Second
    | _ -> failwith (sprintf "expected s,y,m,d,h,n, or c but saw %c" c)
        
(** Return [num_field] for given [c] assuming [c] occurred immediately
    after a '%' symbol. *)
let parse_num_field (c:char) : num_field =
  match c with
    | 'y' -> Year
    | 'm' -> Month
    | 'd' -> Day
    | 'h' -> Hour
    | 'n' -> Minute
    | 'c' -> Second
    | _ -> failwith (sprintf "expected y,m,d,h,n, or c but saw %c" c)

let num_field_to_char (nfield : num_field) : char =
  match nfield with
    | Year -> 'y'
    | Month -> 'm'
    | Day -> 'd'
    | Hour -> 'h'
    | Minute -> 'n'
    | Second -> 'c'

let elem_to_string = function
  | Char c -> sprintf "%c" c
  | String -> "%s"
  | DefaultNum nfield -> sprintf "%%%c" (num_field_to_char nfield)
  | ExactNum (n,nfield) -> sprintf "%%%d%c" n (num_field_to_char nfield)
  | DeltaNum (Plus,n,nfield) -> sprintf "%%+%d%c" n (num_field_to_char nfield)
  | DeltaNum (Minus,n,nfield) -> sprintf "%%-%d%c" n (num_field_to_char nfield)

let to_string (t:t) : string =
  String.concat "" (List.map elem_to_string t)
