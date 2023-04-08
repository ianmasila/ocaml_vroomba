(******************************)
(*  Read a file into a string *)
(******************************)

(* Reading *)

let read_file_to_strings (filename : string) : string list =
  let open Core in
  let file = In_channel.create filename in
  let strings = In_channel.input_lines file in
  In_channel.close file;
  strings

let read_file_to_single_string (filename : string) : string =
  let open Core in
  In_channel.with_file filename ~f:(fun input -> In_channel.input_all input)

(* Writing *)

let write_string_to_file (filename : string) (text : string) : unit =
  let open Core in
  let outc = Out_channel.create ~append:false ~fail_if_exists:false filename in
  Out_channel.output_string outc text;
  Out_channel.close outc

let write_strings_to_file (filename : string) (lines : string list) : unit =
  let open Core in
  Out_channel.with_file ~append:false ~fail_if_exists:false filename ~f:(fun out ->
      List.iter lines ~f:(fun s -> Out_channel.fprintf out "%s\r\n" s))

(*
Checking consistency of the file

cksum filename
md5 filename

*)

(*  Getting file size  *)
let file_size (filename : string) : int =
  let ic = open_in filename in
  let len = in_channel_length ic in
  close_in ic;
  len

(*************************************)
(*         Character Encodings       *)
(*************************************)

let ascii_string : string = "ATR"

let utf16_string : string = "ATЯ"

(* Parsing strings from files *)

let trimmer : string -> string =
  Core.String.strip ~drop:(fun c ->
      Core.List.mem ['\n'; ' '; '\r'] c ~equal:(fun a b -> a == b))

let splitter (s : string) : string list =
  let open Core in
  String.split_on_chars ~on:['\n'; ' '; '\r'] s
  |> List.filter ~f:(fun s -> not @@ String.is_empty s)
