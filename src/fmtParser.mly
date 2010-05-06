%{
%}

%token PLUS MINUS PERCENT EOF
%token <char> CHAR
%token <int> INTEGER

%start input
%type <Fmt.t> input

%%
input:
| fmt EOF {$1}

fmt:
| elem {$1}
| elem fmt {$1@$2}
;

elem:
| CHAR {[Fmt.Char $1]}
| PLUS {[Fmt.Char '+']}
| MINUS {[Fmt.Char '-']}
| INTEGER {List.map (fun c -> Fmt.Char c) (Fmt.explode (string_of_int $1))}
| PERCENT CHAR {[Fmt.parse_percent_char $2]}
| PERCENT INTEGER num_field {[Fmt.ExactNum ($2,$3)]}
| PERCENT PLUS INTEGER num_field {[Fmt.DeltaNum (Fmt.Plus,$3,$4)]}
| PERCENT MINUS INTEGER num_field {[Fmt.DeltaNum (Fmt.Minus,$3,$4)]}
;

num_field:
| CHAR {Fmt.parse_num_field $1}

%%
