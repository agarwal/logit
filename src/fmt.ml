(** Base format strings. Format strings for both scanning and printing
    use this as their internal representation. *)

open Printf;; open CalendarLib

(** Types of numeric format specifiers. *)
type num_spec = YearSpec | MonthSpec | DaySpec | HourSpec | MinuteSpec | SecondSpec

type pm = Plus | Minus

(** Each format specifier represents how to parse or print a specific
    type of value. *)
type spec =
    | CharSpec of char
    | StringSpec
    | DefaultNumSpec of num_spec
    | ExactNumSpec of int * num_spec
    | DeltaNumSpec of pm * int * num_spec
        
(** A format string is an ordered list of format specifiers. *)
type t = spec list
    
(** True if given [spec] represents a numeric format specifiers. *)
let is_num_spec (x:spec) : bool =
  match x with
    | CharSpec _
    | StringSpec -> false
    | DefaultNumSpec _
    | ExactNumSpec _
    | DeltaNumSpec _ -> true

(** Convert given [char] to its corresponding [num_spec]. *)
let char_to_num_spec (c:char) : num_spec =
  match c with
    | 'y' -> YearSpec
    | 'm' -> MonthSpec
    | 'd' -> DaySpec
    | 'h' -> HourSpec
    | 'n' -> MinuteSpec
    | 'c' -> SecondSpec
    | _ -> failwith (sprintf "expected y,m,d,h,n, or c but saw %c" c)
        
(** Return [spec] corresponding to the string "%[x]", that is a
    percent symbol followed by the character [x]. *)
let char_to_spec (x:char) : spec =
  match x with
    | 's' -> StringSpec
    | 'y' | 'm' | 'd' | 'h' | 'n' | 'c' -> DefaultNumSpec (char_to_num_spec x)
    | _ -> failwith (sprintf "expected s,y,m,d,h,n, or c but saw %c" x)
        
(** Convert given [num_spec] to its corresponding [char]. *)
let num_spec_to_char (nfield : num_spec) : char =
  match nfield with
    | YearSpec -> 'y'
    | MonthSpec -> 'm'
    | DaySpec -> 'd'
    | HourSpec -> 'h'
    | MinuteSpec -> 'n'
    | SecondSpec -> 'c'

let spec_to_string = function
  | CharSpec c -> sprintf "%c" c
  | StringSpec -> "%s"
  | DefaultNumSpec nfield -> sprintf "%%%c" (num_spec_to_char nfield)
  | ExactNumSpec (n,nfield) -> sprintf "%%%d%c" n (num_spec_to_char nfield)
  | DeltaNumSpec (Plus,n,nfield) -> sprintf "%%+%d%c" n (num_spec_to_char nfield)
  | DeltaNumSpec (Minus,n,nfield) -> sprintf "%%-%d%c" n (num_spec_to_char nfield)

let to_string (t:t) : string =
  String.concat "" (List.map spec_to_string t)
    
(** Types of values associated with format specifiers of each possible
    [spec_type]. Format specifiers of CharType can have 'Char c' data,
    those of StringType can have 'String s' data, and so on. *)
type spec_data =
    | Char of char
    | String of string
    | Year of int
    | Month of int
    | Day of int
    | Hour of int
    | Minute of int
    | Second of int
