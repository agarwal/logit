{
open FmtParser
let num_chars = ref 0
}

let digits = ['0'-'9']+

rule lex = parse
  | '%'         {incr num_chars; PERCENT}
  | '+'         {incr num_chars; PLUS}
  | '-'         {incr num_chars; MINUS}
  | digits as x {num_chars := !num_chars + (String.length x);
                 INTEGER (int_of_string x)}
  | eof         {EOF}
  | _ as c      {incr num_chars; CHAR c}

{
}
