open Util.Utility
(* Initial estimates for input size *)
let default_lines = 100
and default_chars = 8000

let trim_initial_newline s =
  let len = String.length s in
  if len > 0 && s.[0] = '\n' then StringLabels.sub s ~pos:1 ~len:(len-1)
  else s

class source_code =
object (self)
  val lines =
    let tbl = Hashtbl.create default_lines in
    Hashtbl.add tbl 0 0;
    tbl
  val text = Buffer.create default_chars

  (* Return the portion of source code that falls between two positions *)
  method private extract_substring (start : Lexing.position) (finish : Lexing.position) =
    try
      Buffer.sub text start.Lexing.pos_cnum (finish.Lexing.pos_cnum - start.Lexing.pos_cnum)
    with Invalid_argument _ -> "*** DUMMY POSITION ****"

  (* Return some lines of the source code *)
  method extract_line_range (startline : int) (finishline : int) =
    try
      let start  = Hashtbl.find lines startline
      and finish = (if finishline = Hashtbl.length lines
      (* handle the last line of input *)
                    then Buffer.length text
                    else Hashtbl.find lines finishline)
      in
      trim_initial_newline (Buffer.sub text (start) (finish - start))
    with Not_found -> "<unknown>"

  (* Return one line of the source code *)
  method extract_line (line : int) =
    self#extract_line_range (line - 1) line

  (* Given a function `infun' as required by Lexing.from_function,
     return another such function that stores the text read in `code'.
  *)
  method parse_into (infun : bytes -> int -> int) : bytes -> int -> int =
    fun buffer nchars ->
    let nchars = infun buffer nchars in
    List.iter (fun linepos ->
        Hashtbl.add lines
          (Hashtbl.length lines)
          (linepos + Buffer.length text))
      (find_char (Bytes.sub buffer 0 nchars) '\n');
    Buffer.add_subbytes text buffer 0 nchars;
    nchars

  (* Retrieve the last line of source code read. *)
  method find_line (pos : Lexing.position) : (string * int) =
    (self#extract_line pos.Lexing.pos_lnum,
     abs @@ pos.Lexing.pos_cnum - Hashtbl.find lines (pos.Lexing.pos_lnum -1) - 1)

  (* Create a `lookup function' that given start and finish positions
     returns a resolved position
  *)
  method lookup =
    fun (start, finish) ->
    (start,
     self#extract_line start.Lexing.pos_lnum,
     self#extract_substring start finish)
    
  method extract_all_code () =
    Buffer.contents text
end

module SourceCodeManager = struct
  let source_code_instance = ref (new source_code)

  let get_instance () = !source_code_instance
end

module Lexpos = struct
  type t = Lexing.position
  [@@name "lexpos"]

  let pp fmt lpos =
    Format.fprintf fmt
      "File %s, line %d, char %d" lpos.Lexing.pos_fname lpos.Lexing.pos_lnum lpos.Lexing.pos_cnum

  let show v = Format.asprintf "%a" pp v
end


module Position = struct
  type t = {
    start : Lexpos.t;
    finish : Lexpos.t;
    code : source_code;
  }
  [@@name "position"]

  let pp : Format.formatter -> t -> unit = fun fmt pos ->
    let pp_non_dummy () =
      let file = pos.start.Lexing.pos_fname in
  
      let bold = "\027[1m"
      and reset = "\027[0m" in
  
      Format.fprintf fmt "%sFile %s, " bold file;
      
      let start_line = pos.start.Lexing.pos_lnum in
      let start_char = pos.start.Lexing.pos_cnum - pos.start.Lexing.pos_bol in
      let finish_line = pos.finish.Lexing.pos_lnum in
      let finish_char = pos.finish.Lexing.pos_cnum - pos.finish.Lexing.pos_bol in

      (* Show only first six line if the source code too long *)
      let finish_line =
        if finish_line - start_line > 5 then
          start_line + 5
        else
          finish_line
      in
  
      if start_line = finish_line then
        if start_char = finish_char then
          Format.fprintf fmt "line %d, column %d%s" start_line start_char reset
        else
          Format.fprintf fmt "line %d, columns %d to %d%s" start_line start_char finish_char reset
      else
        Format.fprintf fmt "line %d, column %d, to line %d, column %d%s" start_line start_char finish_line finish_char reset;
  
      let line_number_width = String.length (string_of_int finish_line) in
      let pad_line_number n =
        let line_number_str = string_of_int n in
        let padding = String.make (line_number_width - String.length line_number_str) ' ' in
        padding ^ line_number_str
      in
      (* Adjust this part to include line numbers with padding for alignment *)
      let source_code_str =
        if start_line = finish_line then
          Format.sprintf "%s| %s" (pad_line_number start_line) (pos.code#extract_line start_line)
        else
          let full_string = pos.code#extract_line_range (start_line-1) finish_line in
          let lines = String.split_on_char '\n' full_string in
          List.mapi (fun i line -> Format.sprintf "%s| %s" (pad_line_number (start_line + i)) line) lines
          |> String.concat "\n"
      in

      (* ANSI escape codes for red color *)
      let red = "\027[31m"
      and reset = "\027[0m" in

      (* Generate the marker line with ^ symbols in red *)
      let marker_line =
          String.make (line_number_width + 2 + start_char) ' ' ^ red ^ String.make (finish_char - start_char + 1) '^' ^ reset
      in

      if marker_line <> "" then
        Format.fprintf fmt "\n%s\n%s" source_code_str marker_line
      else
        Format.fprintf fmt "\n%s" source_code_str
    in 
    if pos.start = Lexing.dummy_pos || pos.finish = Lexing.dummy_pos then
      Format.fprintf fmt "<dummy position>"
    else
      pp_non_dummy ()
      
  let show v = Format.asprintf "%a" pp v

  let make ~start ~finish ~code =
    { start; finish; code; }

  let dummy = make ~start:Lexing.dummy_pos ~finish:Lexing.dummy_pos ~code:(SourceCodeManager.get_instance ())

  let start t = t.start

  let finish t = t.finish

  let code t = t.code

  let format_pos pos_list = 
    String.concat "\n " (List.map (fun pos -> Format.asprintf "%a" pp pos) pos_list)
end

module WithPos = struct
  type 'a t = { node : 'a
              ; pos  : (Position.t[@name "position"][@opaque])
              } 
            [@@name "withP"]
            [@@deriving visitors { variety = "map"; polymorphic = true}]
            
  let make ?(pos = Position.dummy) node = { node; pos }
  let dummy node = make node

  let node t = t.node

  let pos t = t.pos

  let pp pp_node ppf { node; pos } =
    Format.fprintf ppf "%a at %a" pp_node node Position.pp pos

  let pp_pos_only fmt { pos; _ } =
    Position.pp fmt pos

  let with_pos pos comp = make ~pos comp

  let extract_pos_pair w1 w2 = [w1.pos; w2.pos]

  let split_with_pos_list lst = 
    ((List.map pos lst),( List.map node lst))
  
  let extract_list_node lst = List.map node lst

  let extract_list_pos lst = List.map pos lst

  let combine_with_pos_list pos_list node_list = 
    List.map2 (fun pos node -> make ~pos node) pos_list node_list

end
