(** A printer specifies how to print output file names. *)

open Printf

(** An index n >= 1 implies that the nth value extracted from the
    input file name should be used. A value of 0 (or negative) implies
    that a 'default' value should be used. *)
type index = int

(** Operators on numeric values. *)
type num_modifier =
    | Plus of int (** [Plus n] adds [n] to a given number *)
    | Minus of int (** [Minus n] subtracts [n] from a given number *)
    | NumIdentity (** return given number unmodified *)

let apply_num_modifier (f : num_modifier) (x : int) : int =
  match f with
    | Plus y -> x + y
    | Minus y -> x - y
    | NumIdentity -> x

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

(** [print_string t data] returns a string formatted according to
    printer [t] and using given [data]. *)
let print_string (printer : t) (data : Data.t) : string =
  let rec loop ans printer =
    match printer with
      | [] -> ans
      | (CharSpec c)::printer -> loop (sprintf "%s%c" ans c) printer
      | (StringSpec n)::printer -> loop (sprintf "%s%s" ans (Data.string n data)) printer
      | (NumSpec (f,x))::printer ->
          let g, size = match x with
            | YearSpec -> Data.year, 4
            | MonthSpec -> Data.month, 2
            | DaySpec -> Data.day, 2
            | HourSpec -> Data.hour, 2
            | MinuteSpec -> Data.minute, 2
            | SecondSpec -> Data.second, 2
          in
          let y = apply_num_modifier f (g data) in
          let y = Util.prepad size '0' (string_of_int y) in
          loop (sprintf "%s%s" ans y) printer
  in
  loop "" printer
