%{
open Printf;; open Printer

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

%}

%token PLUS MINUS PERCENT EOF
%token <char> CHAR
%token <int> INTEGER

%start make_printer
%type <Printer.t> make_printer

%%
make_printer:
| fmt EOF {$1}

fmt:
| elem {$1}
| elem fmt {$1@$2}
;

elem:
| CHAR {[CharSpec $1]}
| PLUS {[CharSpec '+']}
| MINUS {[CharSpec '-']}
| INTEGER {List.map (fun c -> CharSpec c) (Util.explode (string_of_int $1))}
| PERCENT CHAR {
    [match $2 with
      | 's' -> StringSpec 1
      | 'y' | 'm' | 'd' | 'h' | 'n' | 'c' -> NumSpec (NumIdentity, char_to_num_spec $2)
      | _ -> failwith (sprintf "expected s,y,m,d,h,n, or c but saw %c" $2)
    ]
  }
| PERCENT INTEGER CHAR {
    [if $2 >= 1 && $3 = 's' then
      StringSpec $2
      else
        failwith (sprintf "invalid format specifier %%%d%c" $2 $3)
    ]
  }
| PERCENT PLUS INTEGER num_field {[NumSpec (Plus $3, $4)]}
| PERCENT MINUS INTEGER num_field {[NumSpec (Minus $3, $4)]}
;

num_field:
| CHAR {char_to_num_spec $1}

%%
