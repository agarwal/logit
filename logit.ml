#!/usr/bin/env ocamlscript
(*********************************************************************************)
(* Copyright (c) 2010 Ashish Agarwal                                             *)
(*                                                                               *)
(* Permission is hereby granted, free of charge, to any person obtaining a copy  *)
(* of this software and associated documentation files (the "Software"), to deal *)
(* in the Software without restriction, including without limitation the rights  *)
(* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell     *)
(* copies of the Software, and to permit persons to whom the Software is         *)
(* furnished to do so, subject to the following conditions:                      *)
(*                                                                               *)
(* The above copyright notice and this permission notice shall be included in    *)
(* all copies or substantial portions of the Software.                           *)
(*                                                                               *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR    *)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,      *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE   *)
(* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER        *)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, *)
(* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN     *)
(* THE SOFTWARE.                                                                 *)
(*********************************************************************************)

Ocaml.packs := ["calendar"; "getopt"; "extlib"]
--
open Printf;; open CalendarLib;; open ExtString

let prog = Filename.basename Sys.argv.(0)

let usage = sprintf
"Usage:
  %s -d out_dir file

Moves file (or directory) to out_dir after prepending its
name with the today's date. See options below to customize
behavior. See README file for examples at the project's
website: http://github.com/agarwal/logit.

Options:
  -d out_dir
   The directory into which the file will be moved.

  -p prefix
  -i infix
  -s suffix
   Output file name is constructed by concatenating the
   prefix, infix, and suffix. Default prefix is today's date
   in the format YYYY-MM-DD, which makes lexicographic and
   chronological sorting equivalent. Default infix is a
   single space character. Default suffix is the input file
   name.

   The suffix includes the file extension (e.g. .txt, .pdf)
   so be sure to include this when overriding the default.
 
  -h
  --help
   Print this help message."
  prog

type params = {
  in_file : string; (* full path *)
  out_dir : string;
  out_file : string (* base file name only *)
}

type options = {
  mutable option_d : string option;
  mutable option_p : string option;
  mutable option_i : string option;
  mutable option_s : string option;
  mutable option_in_file : string option;
  mutable option_help : bool
}

let options_to_params (t:options) : params =
  if t.option_help then
    (printf "%s\n" usage; exit 0)
  ;

  let in_file = match t.option_in_file with
    | None -> failwith "must specify input file"
    | Some x ->
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
  
  let out_dir = match t.option_d with
    | None -> failwith "must specify output directory"
    | Some x ->
        if not (Sys.file_exists x) then
          failwith (sprintf "%s: no such directory" x)
        else if not (Sys.is_directory x) then
          failwith (sprintf "%s: not a directory" x)
        else
          x
  in

  let prefix = match t.option_p with
    | None -> Printer.Date.sprint "%Y-%m-%d" (Date.today())
    | Some x -> x
  in

  let infix = match t.option_i with
    | None -> " "
    | Some x -> x
  in
  
  let suffix = match t.option_s with
    | None -> Filename.basename in_file
    | Some x -> x
  in
  
  let out_file = 
    let ans = sprintf "%s%s%s" prefix infix suffix in
    let ans' = Filename.concat out_dir ans in
    if Sys.file_exists ans' then
      failwith (sprintf "%s: will not overwrite existing file" ans')
    else
      ans
  in
  {
    in_file = in_file;
    out_dir = out_dir;
    out_file = out_file
  }

let parse_cmdline() : params =
  let t = {option_d=None; option_p=None; option_i=None; option_s=None; option_in_file=None; option_help=false} in

  let opts = [
    'd', "", None, Some (fun x -> t.option_d <- Some x);
    'p', "", None, Some (fun x -> t.option_p <- Some x);
    'i', "", None, Some (fun x -> t.option_i <- Some x);
    's', "", None, Some (fun x -> t.option_s <- Some x);
    'h', "help", Some (fun () -> t.option_help <- true), None;
  ]
  in

  let set_option_in_file x = match t.option_in_file with
    | None -> t.option_in_file <- Some x
    | Some _ -> failwith (sprintf "multiple input files not allowed")
  in
  
  Getopt.parse_cmdline opts set_option_in_file;
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
  let in_file = escape_spaces p.in_file in
  let out_file = escape_spaces (Filename.concat p.out_dir p.out_file) in
  let cmd = sprintf "mv %s %s" in_file out_file in
  printf "%s\n" cmd;
  exit (Sys.command cmd)
with
    Failure msg | Getopt.Error msg -> eprintf "%s\n" msg
