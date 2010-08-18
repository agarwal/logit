(** Data values associated with scanner specifiers. *)

open Printf;; open Util

(** Types of values associated with scanner specifiers of each possible
    [Scanner.spec]. For example:
    - [CharSpec c] has 'Char c' data. This is a trivial case since the
    only data associated by the [CharSpec c] specifier is the
    character [c].
    - [StringSpec] has 'String s' data, where [s] is the specific
    string that was parsed.
    - [YearSpec] has 'Year n' data, where [n] is the
    specific year that was parsed.
    - etc.
*)
type datum =
    | Char of char
    | String of string
    | Year of int
    | Month of int
    | Day of int
    | Hour of int
    | Minute of int
    | Second of int

(** Data associated with scanner. There is one data value for each
    specifier in the scanner. *)
type t = datum list

type typ =
    | CharType
    | StringType
    | YearType
    | MonthType
    | DayType
    | HourType
    | MinuteType
    | SecondType

let typeOfDatum (d:datum) : typ =
  match d with
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

let year t : int =
  match find_first (fun a -> typeOfDatum a = YearType) t with
    | Some (Year x) -> x
    | Some _ -> assert false
    | None -> (time_now()).Unix.tm_year + 1900

let month t : int =
  match find_first (fun a -> typeOfDatum a = MonthType) t with
    | Some (Month x) -> x
    | Some _ -> assert false
    | None -> (time_now()).Unix.tm_mon + 1

let day t : int =
  match find_first (fun a -> typeOfDatum a = DayType) t with
    | Some (Day x) -> x
    | Some _ -> assert false
    | None -> (time_now()).Unix.tm_mday

let hour t : int =
  match find_first (fun a -> typeOfDatum a = HourType) t with
    | Some (Hour x) -> x
    | Some _ -> assert false
    | None -> (time_now()).Unix.tm_hour

let minute t : int =
  match find_first (fun a -> typeOfDatum a = MinuteType) t with
    | Some (Minute x) -> x
    | Some _ -> assert false
    | None -> (time_now()).Unix.tm_min

let second t : int =
  match find_first (fun a -> typeOfDatum a = SecondType) t with
    | Some (Second x) -> x
    | Some _ -> assert false
    | None -> (time_now()).Unix.tm_sec


(** {6 Char and String Accessors} *)

let char t : char =
  match find_first (fun a -> typeOfDatum a = CharType) t with
    | Some (Char x) -> x
    | Some _ -> assert false
    | None -> failwith "Char data not found"

(** Returns [n]th String data value in given data. *)
let string n t : string =
  let t = List.filter (fun a -> typeOfDatum a = StringType) t in
  let t = Array.of_list t in
  let l = Array.length t in
  if l >= n then
    match t.(n-1) with
      | String x -> x
      | _ -> assert false
  else
    failwith (sprintf "have only %d string values but asked for %d" l n)
