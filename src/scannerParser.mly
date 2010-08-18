%{
open Printf;; open Scanner
%}

%token PERCENT EOF
%token <char> CHAR

%start make_scanner
%type <Scanner.t> make_scanner

%%
make_scanner:
| fmt EOF {$1}

fmt:
| elem {$1}
| elem fmt {
    match ($1, $2) with
      | (_, [])
      | ([], _)
      | (_::_::_, _) -> failwith "impossible to get here"
      | (StringSpec::[], StringSpec::_) ->
          failwith "multiple consecutive %s format specifiers not allowed"
      | (_, _) -> $1@$2
  }
;

elem:
| CHAR {[CharSpec $1]}
| PERCENT CHAR {
    [match $2 with
      | 's' -> StringSpec
      | 'y' -> YearSpec
      | 'm' -> MonthSpec
      | 'd' -> DaySpec
      | 'h' -> HourSpec
      | 'n' -> MinuteSpec
      | 'c' -> SecondSpec
      | _ -> failwith (sprintf "expected s,y,m,d,h,n, or c but saw %c" $2)
    ]
  }

%%
