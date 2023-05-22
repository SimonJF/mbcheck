{
(* Preamble *)
open Lexing
open Parser
open Util

exception SyntaxError of string

(* Increments internal lexer metadata *)
let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
        { pos with pos_bol = lexbuf.lex_curr_pos;
                   pos_lnum = pos.pos_lnum + 1
        }


(* Keywords *)
let keywords = [
    "let", LET;
    "spawn", SPAWN;
    "new", NEW;
    "guard", GUARD;
    "receive", RECEIVE;
    "free", FREE;
    "fail", FAIL;
    "in", IN;
    "linfun", LINFUN;
    "fun", FUN;
    "from", FROM;
    "interface", INTERFACE;
    "def", DEF;
    "if", IF;
    "else", ELSE;
    "case", CASE;
    "of", OF;
    "inl", INL;
    "inr", INR
]
}

(* Regular expressions (mostly taken from Links) *)
let def_id = (['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '_' '0'-'9' '\'']*)
let def_white = [' ' '\t']+
let def_newline = '\r' | '\n' | "\r\n"
let def_integer = (['1'-'9'] ['0'-'9']* | '0')
let def_float = (def_integer '.' ['0'-'9']+ ('e' ('-')? def_integer)?)
let op_char = [ '.' '!' '$' '&' '*' '+' '/' '<' '=' '>' '@' '\\' '^' '-' '|' ]

(* Lexing rules *)

rule read =
    parse
    | def_white  { read lexbuf }
    | def_newline { next_line lexbuf; read lexbuf }
    | def_integer { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | '#' ([^ '\n'] *) { read lexbuf } (* Comment *)
    (* | def_float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }  *)
    | "true"   { BOOL true }
    | "false"  { BOOL false }
    | def_id as var { try List.assoc var keywords
                          with Not_found ->
                          if Utility.is_uppercase var.[0] then
                              CONSTRUCTOR var
                          else
                              VARIABLE var }
    | '"'      { read_string (Buffer.create 17) lexbuf }
    | '{'      { LEFT_BRACE }
    | '}'      { RIGHT_BRACE }
    | '['      { LEFT_BRACK }
    | ']'      { RIGHT_BRACK }
    | '('      { LEFT_PAREN }
    | ')'      { RIGHT_PAREN }
    | ';'      { SEMICOLON }
    | ':'      { COLON }
    | ','      { COMMA }
    | '='      { EQ }
    | '!'      { BANG }
    | '?'      { QUERY }
    | '.'      { DOT }
    | '*'      { STAR }
    | '/'      { DIV }
    | '+'      { PLUS }
    | '-'      { MINUS }
    | '|'      { PIPE }
    | "&&"     { AND }
    | "||"     { OR }
    | ">="     { GEQ }
    | "<"      { LT }
    | ">"      { GT }
    | "<="     { LEQ }
    | "=="     { EQQ }
    | "!="     { NEQ }
    | "->"     { RIGHTARROW }
    | "-o"     { LOLLI }
    | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
    | eof      { EOF }
and read_string buf =
    parse
    | '"'       { STRING (Buffer.contents buf) }
    | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
    | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
    | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
    | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
    | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
    | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
    | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
    | [^ '"' '\\']+
      { Buffer.add_string buf (Lexing.lexeme lexbuf);
        read_string buf lexbuf
      }
    | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
    | eof { raise (SyntaxError ("String is not terminated")) }
