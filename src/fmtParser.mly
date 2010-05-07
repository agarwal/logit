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
| CHAR {[Fmt.CharSpec $1]}
| PLUS {[Fmt.CharSpec '+']}
| MINUS {[Fmt.CharSpec '-']}
| INTEGER {List.map (fun c -> Fmt.CharSpec c) (Util.explode (string_of_int $1))}
| PERCENT CHAR {[Fmt.char_to_spec $2]}
| PERCENT INTEGER num_field {[Fmt.ExactNumSpec ($2,$3)]}
| PERCENT PLUS INTEGER num_field {[Fmt.DeltaNumSpec (Fmt.Plus,$3,$4)]}
| PERCENT MINUS INTEGER num_field {[Fmt.DeltaNumSpec (Fmt.Minus,$3,$4)]}
;

num_field:
| CHAR {Fmt.char_to_num_spec $1}

%%
