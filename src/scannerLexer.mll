{
open ScannerParser
let num_chars = ref 0
}

rule lex = parse
  | '%'         {incr num_chars; PERCENT}
  | eof         {EOF}
  | _ as c      {incr num_chars; CHAR c}

{
}
