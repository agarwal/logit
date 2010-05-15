open Printf;; open Util;; open Fmt

type t_to_break_recursion = t
type t = t_to_break_recursion
    
let make fmt =
  let rec validate fmt : unit =
    match fmt with
      | [] -> ()
      | StringSpec::StringSpec::_ ->
          failwith "multiple consecutive %s format specifiers not allowed"
      | (CharSpec _)::fmt
      | StringSpec::fmt
      | (DefaultNumSpec _)::fmt -> validate fmt
      | (ExactNumSpec _ as spec)::_
      | (DeltaNumSpec _ as spec)::_ ->
          failwith (sprintf "%s: invalid scanner" (spec_to_string spec))
  in
  validate fmt; fmt

type parse_result =
    | Success of spec_data * string (* result of parsing one spec and remaining string *)
    | NoParse of string (* error message explaining failure *)

(* Parse string according to given format specifier, which must not be
   StringSpec. *)
let parseAnySpecExceptString (spec:spec) (str:string) : parse_result =
  match spec with
    | CharSpec c ->
        if String.length str = 0 then
          NoParse (sprintf "expected %c but reached end-of-input" c)
        else if c <> str.[0] then
          NoParse (sprintf "expected %c but saw %c" c str.[0])
        else
          Success (Char c, string_drop 1 str)
    | DefaultNumSpec YearSpec -> (
        let n = String.length str in
        if n < 4 then
          NoParse "expected 4 digit year but reached end-of-input"
        else
          try
            let x = int_of_string (string_take 4 str) in
            Success (Year x, string_drop 4 str)
          with
              Failure _ -> NoParse (sprintf "%s: invalid year" str)
      )
    | DefaultNumSpec MonthSpec -> (
        let n = String.length str in
        if n < 2 then
          NoParse "expected 2 digit month but reached end-of-input"
        else
          try
            let x = int_of_string (string_take 2 str) in
            if 1 <= x && x <= 12 then 
              Success (Month x, string_drop 2 str)
            else
              NoParse (sprintf "%d: invalid month" x)
          with
              Failure _ -> NoParse (sprintf "%s: invalid month" str)
      )
    | DefaultNumSpec DaySpec -> (
        let n = String.length str in
        if n < 2 then
          NoParse "expected 2 digit day but reached end-of-input"
        else
          try
            let x = int_of_string (string_take 2 str) in
            if 1 <= x && x <= 31 then 
              Success (Day x, string_drop 2 str)
            else
              NoParse (sprintf "%d: invalid day" x)
          with
              Failure _ -> NoParse (sprintf "%s: invalid day" str)
      )
    | DefaultNumSpec HourSpec -> (
        let n = String.length str in
        if n < 2 then
          NoParse "expected 2 digit hour but reached end-of-input"
        else
          try
            let x = int_of_string (string_take 2 str) in
            if 0 <= x && x <= 23 then 
              Success (Hour x, string_drop 2 str)
            else
              NoParse (sprintf "%d: invalid hour" x)
          with
              Failure _ -> NoParse (sprintf "%s: invalid hour" str)
      )
    | DefaultNumSpec MinuteSpec -> (
        let n = String.length str in
        if n < 2 then
          NoParse "expected 2 digit minute but reached end-of-input"
        else
          try
            let x = int_of_string (string_take 2 str) in
            if 1 <= x && x <= 60 then 
              Success (Minute x, string_drop 2 str)
            else
              NoParse (sprintf "%d: invalid minute" x)
          with
              Failure _ -> NoParse (sprintf "%s: invalid minute" str)
      )
    | DefaultNumSpec SecondSpec -> (
        let n = String.length str in
        if n < 2 then
          NoParse "expected 2 digit seconds but reached end-of-input"
        else
          try
            let x = int_of_string (string_take 2 str) in
            if 1 <= x && x <= 60 then
              Success (Second x, string_drop 2 str)
            else
              NoParse (sprintf "%d: invalid seconds" x)
          with
              Failure _ -> NoParse (sprintf "%s: invalid seconds" str)
      )
    | ExactNumSpec _
    | DeltaNumSpec _
    | StringSpec _ -> assert false

(* Return true if [spec], which must not be StringSpec, can
   successfully parse beginning of given string. *)
let specDoesParse (spec:spec) (str:string) : bool =
  match parseAnySpecExceptString spec str with
    | NoParse _ -> false
    | Success _ -> true

let parse scanner str =
  let rec loop ans scanner str =
    match scanner with
      | []  (* same as ending with %s *)
      | StringSpec::[] -> (String str)::ans
      | (ExactNumSpec _)::_
      | (DeltaNumSpec _)::_
      | StringSpec::StringSpec::_ -> assert false
      | (CharSpec _ as spec)::scanner
      | (DefaultNumSpec _ as spec)::scanner -> (
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
          loop ((String chars)::ans) scanner str
  in
  List.rev (loop [] scanner str)
