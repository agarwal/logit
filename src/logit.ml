open Printf;; open ExtString

let prog = Filename.basename Sys.argv.(0)

let usage = sprintf
"Usage:
  %s [-i scanner] -o printer [-d out_dir] file1 file2 ...

Rename files according to format specified by given printer,
which includes special directives for date/time-stamping.
Optionally move the file to another directory if out_dir is
specified. A scanner can also be provided to use parts of the
input file name in the output file name.

Summary of options is below, but you will need to read
documentation at

  http://github.com/agarwal/logit

for an explanation of the scanner and printer format
strings.

Options:
  -i scanner
   Parse input file name according to scanner if given.
   Default scanner is \"%%s\".

  -o printer
   Specifies how to \"print\" name of the output file.

  -d out_dir
   The directory into which the file will be moved. Default
   is to rename in current directory.

  -h
  --help
   Print this help message.

Examples:

Assuming today's date is August 15, 2010, the following pairs
of commands are equivalent:

  %s -o \"%%1s %%y-%%m-%%d\" notes.txt
  mv notes.txt 2010-08-15\\ notes.txt

Use a scanner to reformat a file name that already contained
a date-stamp:

  %s -i \"%%s%%y%%m%%d\" -o \"%%1y-%%1m-%%1d_%%s\" notes20100613.txt
  mv notes20100613.txt 2010-06-13_notes.txt"
  prog prog prog

type params = {
  scanner : Scanner.t;
  printer : Printer.t;
  in_files : string list; (* full paths *)
  out_dir : string; (* possibly empty string *)
}

type options = {
  mutable option_i : string option;
  mutable option_o : string option;
  mutable option_d : string option;
  mutable option_in_files : string list;
  mutable option_help : bool
}

let options_to_params (t:options) : params =
  if t.option_help then
    (printf "%s\n" usage; exit 0)
  ;

  let in_files =
    let process_filename x =
      let n = String.length x in
      if n = 0 then
        failwith (sprintf "%s: invalid empty file or directory name" x)
      else
        let c = x.[n-1] in
        if c = '.' then
          failwith (sprintf "%s: invalid final character in file or directory" x)
        else if not (Sys.file_exists x) then
          failwith (sprintf "%s: no such file or directory" x)
        else if c = '/' || c = '\\' then
          String.rchop x
        else
          x
    in
    List.map process_filename t.option_in_files
  in
  
  let out_dir = match t.option_d with
    | None -> ""
    | Some x ->
        if not (Sys.file_exists x) then
          failwith (sprintf "%s: no such directory" x)
        else if not (Sys.is_directory x) then
          failwith (sprintf "%s: not a directory" x)
        else
          x
  in

  let scanner = 
    let ans = match t.option_i with None -> "%s" | Some x -> x in
    try ScannerParser.make_scanner ScannerLexer.lex (Lexing.from_string ans)
    with
      | Parsing.Parse_error ->
          failwith (sprintf "at character %d: unexpected symbol(s)" !ScannerLexer.num_chars)
      | Failure msg ->
          failwith (sprintf "at character %d: %s" !ScannerLexer.num_chars msg)
  in

  let printer =
    let ans = match t.option_o with None -> "%y-%m-%d %s" | Some x -> x in
    try PrinterParser.make_printer PrinterLexer.lex (Lexing.from_string ans) with
      | Parsing.Parse_error -> 
          failwith (sprintf "at character %d: unexpected symbol(s)\n" !PrinterLexer.num_chars)
      | Failure msg ->
          failwith (sprintf "at character %d: %s\n" !PrinterLexer.num_chars msg)
  in
  {
    scanner = scanner;
    printer = printer;
    in_files = in_files;
    out_dir = out_dir;
  }

let parse_cmdline() : params =
  let t = {option_i=None; option_o=None; option_d=None; option_in_files=[]; option_help=false} in

  let opts = [
    'i', "", None, Some (fun x -> t.option_i <- Some x);
    'o', "", None, Some (fun x -> t.option_o <- Some x);
    'd', "", None, Some (fun x -> t.option_d <- Some x);
    'h', "help", Some (fun () -> t.option_help <- true), None;
  ]
  in

  let append_in_file x = t.option_in_files <- x::t.option_in_files in
  Getopt.parse_cmdline opts append_in_file;
  options_to_params t
    
let escape_spaces (s:string) : string =
  let f c ans = match c with
    | ' ' -> '\\'::' '::ans
    | _ -> c::ans
  in
  String.implode (String.fold_right f s [])
    
;;
try
  let p = parse_cmdline() in

  let run in_file =
    try
      let basename = Filename.basename in_file in
      let base,ext = Util.base_ext basename in
      let data = Scanner.parse_string p.scanner base in
      
      let out_file = Printer.print_string p.printer data in
      let out_file = Filename.concat p.out_dir (out_file ^ ext) in
      let cmd = sprintf "mv %s %s" (escape_spaces in_file) (escape_spaces out_file) in
      printf "%s\n" cmd;
      ignore (Sys.command cmd)
    with
        Failure msg -> eprintf "%s: %s\n" in_file msg
  in
  
  List.iter run p.in_files  

with
    Getopt.Error msg -> eprintf "%s\n" msg
