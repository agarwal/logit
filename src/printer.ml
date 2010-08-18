(** A printer specifies how to print output file names. *)

open Printf

(** Represents index into an array of values. First item is numbered 1
    so value must be >= 1. *)
type index = int

(** Operators on numeric values. *)
type num_modifier =
    | Plus of int (** [Plus n] adds [n] to a given number *)
    | Minus of int (** [Minus n] subtracts [n] from a given number *)
    | NumIdentity (** return given number unmodified *)

(** Types of numeric format specifiers. *)
type num_spec = YearSpec | MonthSpec | DaySpec | HourSpec | MinuteSpec | SecondSpec

(** Each format specifier represents how to print a specific type of
    value. *)
type spec =
    | CharSpec of char
    | StringSpec of index
    | NumSpec of num_modifier * num_spec
        
(** Type of a printer, i.e. a specification of how to print an
    output file name. Also called a format string. *)
type t = spec list

(** [to_string t] returns the string representation of [t]. It is
    roughly the inverse of [of_string] but possibly normalized. Only
    needed for debugging purposes. *)
let to_string (t:t) : string =
  let num_spec_to_char = function
    | YearSpec -> 'y'
    | MonthSpec -> 'm'
    | DaySpec -> 'd'
    | HourSpec -> 'h'
    | MinuteSpec -> 'n'
    | SecondSpec -> 'c'
  in

  let index_to_string x =
    if x = 1 then ""
    else if x > 1 then sprintf "%d" x
    else failwith (sprintf "BUG: invalid index %d" x)
  in

  let num_modifier_to_string = function
    | Plus n -> sprintf "+%d" n
    | Minus n -> sprintf "-%d" n
    | NumIdentity -> ""
  in
  
  let spec_to_string = function
    | CharSpec c -> sprintf "%c" c
    | StringSpec n -> sprintf "%%%ss" (index_to_string n)
    | NumSpec (f,x) -> sprintf "%%%s%c" (num_modifier_to_string f) (num_spec_to_char x)
  in

  String.concat "" (List.map spec_to_string t)

(*

(** [print_string t data] returns a string formatted according to
  printer [t] and using given [data]. *)
let print_string printer data =
  let rec loop ans printer =
    match printer with
      | [] -> ans
      | (CharSpec c)::printer -> loop (sprintf "%s%c" ans c) printer
      | StringSpec::printer -> loop (sprintf "%s%s" ans (Data.string dat)) printer
      | DefaultNumSpec YearSpec ->
          loop (sprintf "%s%d" ans (Data.year dat)) printer
      | DefaultNumSpec MonthSpec ->
          loop (sprintf "%s%d" ans (Data.month dat)) printer
      | ExactNumSpec (n,YearSpec) ->
          loop 
*)
