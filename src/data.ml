(** Data values associated with format specifiers. *)

open Printf

(** Types of values associated with format specifiers of each possible
    type of [spec]. For example:
    - [CharSpec] has 'Char c' data
    - [StringSpec] has 'String s' data
    - [DefaultNumSpec Year] has 'Year n' data
    - etc.
*)
type t =
    | Char of char
    | String of string
    | Year of int
    | Month of int
    | Day of int
    | Hour of int
    | Minute of int
    | Second of int

(** Used only for debugging. *)
let to_string = function
  | Char x -> sprintf "Char %c" x
  | String x -> sprintf "String %s" x
  | Year x -> sprintf "Year %d" x
  | Month x -> sprintf "Month %d" x
  | Day x -> sprintf "Day %d" x
  | Hour x -> sprintf "Hour %d" x
  | Minute x -> sprintf "Minute %d" x
  | Second x -> sprintf "Second %d" x
