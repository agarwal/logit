(** Data values associated with format specifiers. *)

open Printf;; open Util

(** Types of values associated with format specifiers of each possible
    type of [spec]. For example:
    - [CharSpec c] has 'Char c' data. This is a trivial case since the
    only data parseable by the [CharSpec c] format specifier is the
    character [c].
    - [StringSpec] has 'String s' data, where [s] is the specific
    string that was parsed.
    - [DefaultNumSpec Year] has 'Year n' data, where [n] is the
    specific year that was parsed.
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

type typ =
    | CharType
    | StringType
    | YearType
    | MonthType
    | DayType
    | HourType
    | MinuteType
    | SecondType

let typeOfData (t:t) : typ =
  match t with
    | Char _ -> CharType
    | String _ -> StringType
    | Year _ -> YearType
    | Month _ -> MonthType
    | Day _ -> DayType
    | Hour _ -> HourType
    | Minute _ -> MinuteType
    | Second _ -> SecondType

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

(** Returns the current time in the local time zone. *)
let time_now () : Unix.tm =
  Unix.localtime (Unix.time())


(** {6 Time Accessors} *)
(** All the following return the first value requested in given data,
    or the value based on the current time if given data does not contain
    it. *)

let year (ts : t list) : int =
  match find_first (fun a -> typeOfData a = YearType) ts with
    | Some (Year x) -> x
    | Some _ -> assert false
    | None -> (time_now()).Unix.tm_year + 1900

let month (ts : t list) : int =
  match find_first (fun a -> typeOfData a = MonthType) ts with
    | Some (Month x) -> x
    | Some _ -> assert false
    | None -> (time_now()).Unix.tm_mon + 1

let day (ts : t list) : int =
  match find_first (fun a -> typeOfData a = DayType) ts with
    | Some (Day x) -> x
    | Some _ -> assert false
    | None -> (time_now()).Unix.tm_mday

let hour (ts : t list) : int =
  match find_first (fun a -> typeOfData a = HourType) ts with
    | Some (Hour x) -> x
    | Some _ -> assert false
    | None -> (time_now()).Unix.tm_hour

let minute (ts : t list) : int =
  match find_first (fun a -> typeOfData a = MinuteType) ts with
    | Some (Minute x) -> x
    | Some _ -> assert false
    | None -> (time_now()).Unix.tm_min

let second (ts : t list) : int =
  match find_first (fun a -> typeOfData a = SecondType) ts with
    | Some (Second x) -> x
    | Some _ -> assert false
    | None -> (time_now()).Unix.tm_sec


(** {6 Char and String Accessors} *)

let char (ts : t list) : char =
  match find_first (fun a -> typeOfData a = CharType) ts with
    | Some (Char x) -> x
    | Some _ -> assert false
    | None -> failwith "Char data not found"

let string (ts : t list) : string =
  match find_first (fun a -> typeOfData a = StringType) ts with
    | Some (String x) -> x
    | Some _ -> assert false
    | None -> failwith "String data not found"
