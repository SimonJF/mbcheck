%{
open Common
open Sugar_ast
open Common_types
open Source_code
open SourceCodeManager

module type Pos = sig
    (* Type of positions. *)
    type t
    val with_pos : t -> 'a -> 'a WithPos.t
end

module ParserPosition
    : Pos with type t = (Lexpos.t * Lexpos.t) = struct
    (* parser position produced by Menhir *)
    type t = Lexpos.t * Lexpos.t
    (* Convert position produced by a parser to SourceCode position *)
    let pos (start, finish) = 
        let code = get_instance () in
        Position.make ~start ~finish ~code:code
    (* Wrapper around SourceCode.WithPos.make.  Accepts parser positions. *)
    let with_pos (start, finish) v = WithPos.make ~pos:(pos (start, finish)) v
end

let get_start_pos e = Position.start (WithPos.pos e)
let get_end_pos e = Position.finish (WithPos.pos e)

(* Helper function to create an expression/interface/decl with a position *)
let with_pos_from_positions p1 p2 newE = ParserPosition.with_pos (p1, p2) newE

let parse_error x pos_list = Errors.Parse_error (x,pos_list)

let binary_op op_name x1 x2 = App { func = ParserPosition.with_pos ((get_start_pos x1),(get_end_pos x2)) (Primitive op_name); tyargs=[]; args = [x1; x2] }


%}
(* Tokens *)
%token <int> INT
%token <bool> BOOL
%token <string> STRING
%token <string> VARIABLE
%token <string> CONSTRUCTOR
%token <string> ATOM

(* %token NULL *)
%token LEFT_BRACE RIGHT_BRACE
%token LEFT_BRACK RIGHT_BRACK
%token LEFT_PAREN RIGHT_PAREN
%token SEMICOLON
%token COLON
%token COMMA
(*
These will be added in later
%token <string> CONSTRUCTOR
%token <float> FLOAT
*)
%token BANG
%token QUERY
%token DOT
%token STAR
%token EQ
%token EOF
%token RIGHTARROW
%token LOLLI
%token PLUS
%token DIV

(* Keyword tokens *)
%token LET
%token SPAWN
%token NEW
%token GUARD
%token RECEIVE
%token FREE
%token FAIL
%token EMPTY
%token IN
%token FUN
%token LINFUN
%token FROM
%token INTERFACE
%token DEF
%token IF
%token ELSE
%token MINUS
%token GEQ
%token LT
%token GT
%token LEQ
%token EQQ
%token NEQ
%token AND
%token OR
%token CASE
%token OF
%token INL
%token INR
%token PIPE

(* Precedence *)
%left AND OR
%left LT GT GEQ LEQ EQQ NEQ
%left PLUS MINUS
%left STAR DIV

(* Start parsing *)
%start <expr> expr_main
%start <program * source_code > program

%%

message:
    | CONSTRUCTOR LEFT_PAREN separated_list(COMMA, expr) RIGHT_PAREN { ($1, $3) }

message_binder:
    | CONSTRUCTOR LEFT_PAREN separated_list(COMMA, VARIABLE) RIGHT_PAREN { ($1, $3) }

type_annot:
    | COLON ty { $2 }

inl_branch:
    | INL LEFT_PAREN VARIABLE RIGHT_PAREN type_annot RIGHTARROW expr { (($3, $5), $7) }

inr_branch:
    | INR LEFT_PAREN VARIABLE RIGHT_PAREN type_annot RIGHTARROW expr { (($3, $5), $7) }

expr:
    (* Let *)
    | LET VARIABLE type_annot? EQ expr IN expr
        { with_pos_from_positions $startpos $endpos (Let { binder = $2; annot = $3; term = $5; body = $7 }) }
    | LET LEFT_PAREN separated_list(COMMA, VARIABLE) RIGHT_PAREN COLON tuple_annotation EQ basic_expr IN expr
        { with_pos_from_positions $startpos $endpos (LetTuple { binders = $3; term = $8; annot = Some $6; cont = $10 }) }
    | LET LEFT_PAREN separated_list(COMMA, VARIABLE) RIGHT_PAREN EQ basic_expr IN expr
        { with_pos_from_positions $startpos $endpos (LetTuple { binders = $3; term = $6; annot = None; cont = $8 }) }
    | basic_expr SEMICOLON expr { with_pos_from_positions $startpos $endpos (Seq ($1, $3)) }
    | basic_expr COLON ty { with_pos_from_positions $startpos $endpos (Annotate ($1, $3)) }
    | basic_expr { $1 }

expr_list:
    | separated_list(COMMA, expr) { $1 }

tuple_exprs:
    | LEFT_PAREN expr COMMA separated_nonempty_list(COMMA, expr) RIGHT_PAREN { $2 :: $4 }

linearity:
    | FUN    { false }
    | LINFUN { true }

basic_expr:
    | INL LEFT_PAREN expr RIGHT_PAREN { with_pos_from_positions $startpos $endpos ( Inl $3 )}
    | INR LEFT_PAREN expr RIGHT_PAREN { with_pos_from_positions $startpos $endpos ( Inr $3 )}
    | CASE expr OF LEFT_BRACE inl_branch PIPE inr_branch RIGHT_BRACE
        { with_pos_from_positions $startpos $endpos ( Case { term = $2; branch1 = $5; branch2 = $7} )}
    (* New *)
    | NEW LEFT_BRACK param_interface RIGHT_BRACK { with_pos_from_positions $startpos $endpos ( New $3 )}
    (* Spawn *)
    | SPAWN LEFT_BRACE expr RIGHT_BRACE { with_pos_from_positions $startpos $endpos ( Spawn $3 )}
    (* Free *)
    | FREE LEFT_PAREN expr RIGHT_PAREN { with_pos_from_positions $startpos $endpos ( Free $3 )}
    (* Sugared Fail forms *)
    | FAIL LEFT_PAREN expr RIGHT_PAREN LEFT_BRACK ty RIGHT_BRACK { with_pos_from_positions $startpos $endpos ( SugarFail ($3, $6))}
    | tuple_exprs { with_pos_from_positions $startpos $endpos ( Tuple $1 ) }
    (* App *)
    | fact typarams LEFT_PAREN expr_list RIGHT_PAREN
        { with_pos_from_positions $startpos $endpos (
            App {   func = with_pos_from_positions $startpos $endpos ($1);
                    tyargs = $2;
                    args = $4 } 
        )}
    (* Lam *)
    | linearity LEFT_PAREN annotated_var_list RIGHT_PAREN COLON ty LEFT_BRACE expr RIGHT_BRACE
        { with_pos_from_positions $startpos $endpos ( Lam { linear = $1; parameters = $3; result_type = $6; body = $8 } )}
    (* Send *)
    | fact BANG message 
        { with_pos_from_positions $startpos $endpos( 
            Send {  target = with_pos_from_positions $startpos $endpos ($1); 
                    message = $3; 
                    iface = None 
            }
        )}
    (* If-Then-Else *)
    | IF LEFT_PAREN expr RIGHT_PAREN LEFT_BRACE expr RIGHT_BRACE ELSE LEFT_BRACE expr RIGHT_BRACE
        { with_pos_from_positions $startpos $endpos ( If { test = $3; then_expr = $6; else_expr = $10 } )}
    (* Guard *)
    | GUARD basic_expr COLON pat LEFT_BRACE guard+ RIGHT_BRACE
        { with_pos_from_positions $startpos $endpos (
            Guard {
                target = $2;
                pattern = $4;
                guards = $6;
                iface = None
            }
        )}
    | op { with_pos_from_positions $startpos $endpos ( $1 )}

op:
    | basic_expr AND basic_expr { binary_op "&&" $1 $3 }
    | basic_expr OR basic_expr { binary_op "||" $1 $3 }
    | basic_expr EQQ basic_expr { binary_op "==" $1 $3 }
    | basic_expr NEQ basic_expr { binary_op "!=" $1 $3 }
    | basic_expr LT basic_expr { binary_op "<" $1 $3 }
    | basic_expr GT basic_expr { binary_op ">" $1 $3 }
    | basic_expr LEQ basic_expr { binary_op "<=" $1 $3 }
    | basic_expr GEQ basic_expr { binary_op ">=" $1 $3 }
    | basic_expr PLUS basic_expr { binary_op "+" $1 $3 }
    | basic_expr MINUS basic_expr { binary_op "-" $1 $3 }
    | basic_expr STAR basic_expr { binary_op "*" $1 $3 }
    | basic_expr DIV basic_expr { binary_op "/" $1 $3 }
    | fact { $1 }

fact:
    (* Unit *)
    | LEFT_PAREN RIGHT_PAREN { Tuple [] }
    | ATOM { Atom (String.(sub $1 1 (length $1 - 1))) }
    | BOOL   { Constant (Constant.wrap_bool $1) }
    (* Var *)
    | VARIABLE {
        if List.mem_assoc $1 (Lib_types.signatures) then
            Primitive $1
        else
            Var $1
    }
    (* Constant *)
    | INT    { Constant (Constant.wrap_int $1) }
    | STRING { Constant (Constant.wrap_string $1) }
    | LEFT_PAREN expr RIGHT_PAREN { WithPos.node $2 }


guard:
    | FAIL COLON ty { with_pos_from_positions $startpos $endpos ( Fail $3) }
    | EMPTY LEFT_PAREN VARIABLE RIGHT_PAREN RIGHTARROW expr { with_pos_from_positions $startpos $endpos (Empty ($3, $6)) }
    | FREE RIGHTARROW expr { with_pos_from_positions $startpos $endpos (GFree $3) }
    | RECEIVE message_binder FROM VARIABLE RIGHTARROW expr
            { with_pos_from_positions $startpos $endpos (
              let (tag, bnds) = $2 in
              Receive { tag; payload_binders = bnds;
                        mailbox_binder = $4; cont = $6 })
            }

(* Type parser *)

ty_list:
    | separated_nonempty_list(COMMA, ty) { $1 }

(* Note: don't parse a 1-tuple, which doesn't make sense *)
tuple_annotation:
    | LEFT_PAREN ty STAR separated_nonempty_list(STAR, ty) RIGHT_PAREN { $2 :: $4 }

parenthesised_datatypes:
    | LEFT_PAREN RIGHT_PAREN { [] }
    | LEFT_PAREN ty_list RIGHT_PAREN { $2 }

ty:
    | parenthesised_datatypes RIGHTARROW simple_ty  { Type.Fun { linear = false; typarams = []; args = $1; result = $3} }
    | parenthesised_datatypes LOLLI simple_ty       { Type.Fun { linear = true; typarams = []; args = $1; result = $3} }
    | LEFT_PAREN simple_ty PLUS simple_ty RIGHT_PAREN { Type.make_sum_type $2 $4 }
    | tuple_annotation { Type.make_tuple_type $1 }
    | simple_ty { $1 }

typarams:
| LT separated_list(COMMA, ty) GT { $2 }

param_interface:
| CONSTRUCTOR typarams { ($1, $2) }

pat:
| star_pat PLUS pat { Type.Pattern.Plus ($1, $3) }
| star_pat DOT pat  { Type.Pattern.Concat ($1, $3) }
| star_pat          { $1 }

star_pat:
| simple_pat STAR   { Type.Pattern.Many $1 }
| simple_pat        { $1 }

simple_pat:
| CONSTRUCTOR { Type.Pattern.Message $1 }
| INT {
        match $1 with
            | 0 -> Type.Pattern.Zero
            | 1 -> Type.Pattern.One
            | _ -> raise (parse_error "Invalid pattern: expected 0 or 1." 
                            [Position.make ~start:$startpos ~finish:$endpos ~code:!source_code_instance])
    }
| LEFT_PAREN pat RIGHT_PAREN { $2 }

ql:
| LEFT_BRACK CONSTRUCTOR RIGHT_BRACK {
    match $2 with
        | "R" -> Type.Quasilinearity.Returnable
        | "U" -> Type.Quasilinearity.Usable
        | _ -> raise (parse_error "Invalid usage: expected U or R." 
                        [Position.make ~start:$startpos ~finish:$endpos ~code:!source_code_instance])
}

mailbox_ty:
| param_interface BANG simple_pat? ql? {
        let quasilinearity =
            Option.value $4 ~default:(Type.Quasilinearity.Usable)
        in
        Type.(Mailbox {
            capability = Capability.Out;
            interface = $1;
            pattern = $3;
            quasilinearity
        })
    }
    | param_interface QUERY simple_pat? ql? {
        let quasilinearity =
            Option.value $4 ~default:(Type.Quasilinearity.Returnable)
        in
        Type.(Mailbox {
            capability = Capability.In;
            interface = $1;
            pattern = $3;
            quasilinearity
        })
    }

simple_ty:
    | mailbox_ty { $1 }
    | base_ty { $1 }

base_ty:
    | VARIABLE { Type.TVar $1 }
    | CONSTRUCTOR {
        match $1 with
            | "Atom" -> Type.Base Base.Atom
            | "Unit" -> Type.Tuple []
            | "Int" -> Type.Base Base.Int
            | "Bool" -> Type.Base Base.Bool
            | "String" -> Type.Base Base.String
            | _ -> raise (parse_error "Expected Atom, Int, Bool, or String"
                            [Position.make ~start:$startpos ~finish:$endpos ~code:!source_code_instance])
    }

message_ty:
    | CONSTRUCTOR LEFT_PAREN separated_list(COMMA, ty) RIGHT_PAREN { ($1, $3) }

message_list:
    | separated_list(COMMA, message_ty) { $1 }

annotated_var:
    | VARIABLE COLON ty { ($1, $3) }

annotated_var_list:
    | separated_list(COMMA, annotated_var)  { $1 }

interface:
    | INTERFACE param_interface LEFT_BRACE message_list RIGHT_BRACE 
        { with_pos_from_positions $startpos $endpos ( Interface.make (fst $2) (snd $2) $4)  }

decl:
    | DEF VARIABLE typarams LEFT_PAREN annotated_var_list RIGHT_PAREN COLON ty LEFT_BRACE expr
    RIGHT_BRACE {
        with_pos_from_positions $startpos $endpos ( 
        {
          decl_name = $2;
          typarams = $3;
          decl_parameters = $5;
          decl_return_type = $8;
          decl_body = $10
        })
    }

expr_main:
    | expr EOF { $1 }

program:
    | interface* decl* expr? EOF { ({ prog_interfaces = $1; prog_decls = $2; prog_body = $3 }, !source_code_instance) }
