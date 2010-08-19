(** A scanner specifies how to parse an input file name. *)

open Printf;; open Util

(** Each format specifier represents how to scan (i.e. parse) a specific
    type of value. *)
type spec =
    | CharSpec of char
    | StringSpec
    | YearSpec
    | MonthSpec
    | DaySpec 
    | HourSpec
    | MinuteSpec
    | SecondSpec

(** A scanner is an ordered list of scanner specifiers. Multiple %s
    specifiers cannot occur consecutively. *)
type t = spec list

let to_string t : string =
  let spec_to_string = function
    | CharSpec x -> sprintf "%c" x
    | StringSpec -> "%s"
    | YearSpec -> "%y"
    | MonthSpec -> "%m"
    | DaySpec -> "%d"
    | HourSpec -> "%h"
    | MinuteSpec -> "n"
    | SecondSpec -> "c"
  in
  String.concat "" (List.map spec_to_string t)
    

type parse_result =
    | Success of Data.datum * string (* result of parsing one spec and remaining string *)
    | NoParse of string (* error message explaining failure *)

(* Parse string according to given scanner specifier, which must not be
   StringSpec. *)
let parseAnySpecExceptString (spec:spec) (str:string) : parse_result =
  match spec with
    | CharSpec c ->
        if String.length str = 0 then
          NoParse (sprintf "expected %c but reached end-of-input" c)
        else if c <> str.[0] then
          NoParse (sprintf "expected %c but saw %c" c str.[0])
        else
          Success (Data.Char c, string_drop 1 str)
    | YearSpec -> (
        let n = String.length str in
        if n < 4 then
          NoParse "expected 4 digit year but reached end-of-input"
        else
          let x4 = string_take 4 str in
          try
            let x = int_of_string x4 in
            Success (Data.Year x, string_drop 4 str)
          with
              Failure _ -> NoParse (sprintf "%s: invalid year" x4)
      )
    | MonthSpec -> (
        let n = String.length str in
        if n < 2 then
          NoParse "expected 2 digit month but reached end-of-input"
        else
          try
            let x = int_of_string (string_take 2 str) in
            if 1 <= x && x <= 12 then 
              Success (Data.Month x, string_drop 2 str)
            else
              NoParse (sprintf "%d: invalid month" x)
          with
              Failure _ -> NoParse (sprintf "%s: invalid month" str)
      )
    | DaySpec -> (
        let n = String.length str in
        if n < 2 then
          NoParse "expected 2 digit day but reached end-of-input"
        else
          try
            let x = int_of_string (string_take 2 str) in
            if 1 <= x && x <= 31 then 
              Success (Data.Day x, string_drop 2 str)
            else
              NoParse (sprintf "%d: invalid day" x)
          with
              Failure _ -> NoParse (sprintf "%s: invalid day" str)
      )
    | HourSpec -> (
        let n = String.length str in
        if n < 2 then
          NoParse "expected 2 digit hour but reached end-of-input"
        else
          try
            let x = int_of_string (string_take 2 str) in
            if 0 <= x && x <= 23 then 
              Success (Data.Hour x, string_drop 2 str)
            else
              NoParse (sprintf "%d: invalid hour" x)
          with
              Failure _ -> NoParse (sprintf "%s: invalid hour" str)
      )
    | MinuteSpec -> (
        let n = String.length str in
        if n < 2 then
          NoParse "expected 2 digit minute but reached end-of-input"
        else
          try
            let x = int_of_string (string_take 2 str) in
            if 1 <= x && x <= 60 then 
              Success (Data.Minute x, string_drop 2 str)
            else
              NoParse (sprintf "%d: invalid minute" x)
          with
              Failure _ -> NoParse (sprintf "%s: invalid minute" str)
      )
    | SecondSpec -> (
        let n = String.length str in
        if n < 2 then
          NoParse "expected 2 digit seconds but reached end-of-input"
        else
          try
            let x = int_of_string (string_take 2 str) in
            if 1 <= x && x <= 60 then
              Success (Data.Second x, string_drop 2 str)
            else
              NoParse (sprintf "%d: invalid seconds" x)
          with
              Failure _ -> NoParse (sprintf "%s: invalid seconds" str)
      )
    | StringSpec _ -> assert false

(** Return true if [spec], which must not be StringSpec, can
    successfully parse beginning of given string. *)
let specDoesParse (spec:spec) (str:string) : bool =
  match parseAnySpecExceptString spec str with
    | NoParse _ -> false
    | Success _ -> true

(** [parse_string scanner str] parses [str] using [scanner]. Returns
    the values parsed for each format element. Raise [Failure] if
    [str] is not in format specified by [scanner]. *)
let parse_string scanner str : Data.t =
  let rec loop ans scanner str =
    match scanner with
      | [] ->
          if String.length str = 0 then ans
          else failwith (sprintf "%s: saw this beyond expected end-of-input" str)
      | StringSpec::[] -> (Data.String str)::ans
      | StringSpec::StringSpec::_ -> assert false
      | (CharSpec _ as spec)::scanner
      | (YearSpec as spec)::scanner
      | (MonthSpec as spec)::scanner
      | (DaySpec as spec)::scanner
      | (HourSpec as spec)::scanner
      | (MinuteSpec as spec)::scanner
      | (SecondSpec as spec)::scanner -> (
          match parseAnySpecExceptString spec str with
            | NoParse msg -> failwith msg
            | Success (dat,str) ->
                loop (dat::ans) scanner str
        )
      | StringSpec::(spec::_ as scanner) ->
          let rec getchars chars str =
            if String.length str = 0 then
              chars, str
            else if specDoesParse spec str then
              chars, str
            else
              getchars (str.[0]::chars) (string_drop 1 str)
          in
          let chars,str = getchars [] str in
          let chars = implode (List.rev chars) in
          loop ((Data.String chars)::ans) scanner str
  in
  List.rev (loop [] scanner str)
