open Printf

type token_type =
  | INT_WORD
  | FLOAT_WORD
  | INT
  | FLOAT
  | STR
  | IDENT
  | WHILE
  | NEQ
  | MINEQ
  | PLUSEQ
  | LBRACK
  | RBRACK
  | LPAREN
  | RPAREN
  | RETURN
  | SEMI
  | IF
  | PLUS
  | SUB
  | MINUS
  | MULT
  | DIV
  | EQUALS
  | NOT
  | COMMA
  | GT
  | LT
  | DIRECTIVE
  | ANDREF


type token = {
  t_type : token_type;
  value : string option;
}


let string_of_token_type = function
  | INT_WORD -> "INT_WORD"
  | FLOAT_WORD -> "FLOAT_WORD"
  | INT -> "INT"
  | FLOAT -> "FLOAT"
  | STR -> "STR"
  | IDENT -> "IDENT"
  | WHILE -> "WHILE"
  | NEQ -> "NEQ"
  | MINEQ -> "MINEQ"
  | PLUSEQ -> "PLUSEQ"
  | LBRACK -> "LBRACK"
  | RBRACK -> "RBRACK"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | RETURN -> "RETURN"
  | SEMI -> "SEMI"
  | IF -> "IF"
  | PLUS -> "PLUS"
  | SUB -> "SUB"
  | MINUS -> "MINUS"
  | MULT -> "MULT"
  | DIV -> "DIV"
  | EQUALS -> "EQUALS"
  | NOT -> "NOT"
  | COMMA -> "COMMA"
  | GT -> "GT"
  | LT -> "LT"
  | DIRECTIVE -> "DIRECTIVE"
  | ANDREF -> "ANDREF"


let string_of_token t =
  match t.value with
  | Some v -> sprintf "%s(%s)" (string_of_token_type t.t_type) v
  | None -> string_of_token_type t.t_type

let lang_numbers = List.init 10 string_of_int
let lang_numbers_float = "." :: lang_numbers
let lang_first_letter_ident = List.init 26 (fun i -> Char.escaped (char_of_int (i + 65))) @
                              List.init 26 (fun i -> Char.escaped (char_of_int (i + 97))) @
                              ["#"]
let lang_ident = lang_first_letter_ident @ ["_"] @ lang_numbers
let lang_reserved = ["int"; "float"; "while"; "if"; "else"; "void"; "return"]
let lang_pairs = ["("; ")"; "{"; "}"]
let lang_operators = ["&"; "!"; "-"; "+"; "="; "/"; "*"; ">"; "<"]


let get_char () =
  let rec aux () =
    try
      let line = input_line stdin in
      let chars = String.to_seq line |> List.of_seq in
      chars @ aux ()
    with End_of_file -> []
  in
  aux ()


let tokenize () =
  let gen = get_char () in
  let buffer = Buffer.create 16 in
  let tokens = ref [] in
  let ident_flag = ref false in
  let numerical_flag = ref false in
  let str_flag = ref false in
  let append_token t_type value =
    tokens := { t_type; value = Some value } :: !tokens
  in
  let append_simple_token t_type =
    tokens := { t_type; value = None } :: !tokens
  in
  let add_token t_type value =
    append_token t_type value;
    Buffer.clear buffer;
    ident_flag := false;
    numerical_flag := false
  in
  let rec process_chars chars =
    match chars with
    | [] -> ()
    | character :: rest ->
      if not !str_flag && character = '"' then
        str_flag := true
      else if !str_flag then
        if character <> '"' then
          Buffer.add_char buffer character
        else (
          str_flag := false;
          append_token STR (Buffer.contents buffer);
          Buffer.clear buffer
        )
      else if not !ident_flag && List.mem (Char.escaped character) lang_first_letter_ident then (
        ident_flag := true;
        Buffer.add_char buffer character
      )
      else if !ident_flag && List.mem (Char.escaped character) lang_ident then
        Buffer.add_char buffer character
      else if !ident_flag then (
        let value = Buffer.contents buffer in
        if String.contains value '#' then
          append_token DIRECTIVE (String.sub value 1 (String.length value - 1))
        else if not (List.mem value lang_reserved) then
          append_token IDENT value
        else (
          match value with
          | "int" -> append_simple_token INT_WORD
          | "float" -> append_simple_token FLOAT_WORD
          | "while" -> append_simple_token WHILE
          | "if" -> append_simple_token IF
          | "else" -> append_simple_token (Obj.magic 0)
          | "void" -> append_simple_token (Obj.magic 0)
          | "return" -> append_simple_token RETURN
          | _ -> ()
        );
        Buffer.clear buffer;
        ident_flag := false
      );
      if not !str_flag && not !ident_flag then (
        if not !numerical_flag && List.mem (Char.escaped character) lang_numbers then (
          Buffer.add_char buffer character;
          numerical_flag := true
        )
        else if !numerical_flag then (
          if List.mem (Char.escaped character) lang_numbers_float then
            Buffer.add_char buffer character
          else (
            let value = Buffer.contents buffer in
            if String.contains value '.' then
              append_token FLOAT value
            else
              append_token INT value;
            numerical_flag := false;
            Buffer.clear buffer
          )
        )
      );
      if not !str_flag then (
        match character with
        | ';' -> append_simple_token SEMI
        | ',' -> append_simple_token COMMA
        | '+' -> append_simple_token PLUS
        | '-' -> append_simple_token SUB
        | '*' -> append_simple_token MULT
        | '/' -> append_simple_token DIV
        | '!' -> append_simple_token NOT
        | '=' -> append_simple_token EQUALS
        | '>' -> append_simple_token GT
        | '<' -> append_simple_token LT
        | '&' -> append_simple_token ANDREF
        | '(' -> append_simple_token LPAREN
        | ')' -> append_simple_token RPAREN
        | '{' -> append_simple_token LBRACK
        | '}' -> append_simple_token RBRACK
        | _ -> ()
      );
      process_chars rest
  in
  process_chars gen;
  List.rev !tokens


let () =
  let tokens = tokenize () in
  List.iter (fun token -> printf "%s\n" (string_of_token token)) tokens
