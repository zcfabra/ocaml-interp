module Token = struct 
  type token_type  = 
  | ILLEGAL
  | EOF
  (* Identifiers and literals *)
  | IDENT
  | INT 
  | STRING
  (* Operators *)
  | LBRACKET
  | RBRACKET
  | ASSIGN 
  | PLUS
  | MINUS 
  | BANG 
  | ASTERISK 
  | SLASH 
  | LT
  | GT
  | EQ
  | NOT_EQ
  | COMMA
  | SEMICOLON 
  | LPAREN 
  | RPAREN 
  | LBRACE 
  | RBRACE 
  (* Keywords *)
  | FUNCTION
  | LET 
  | TRUE 
  | FALSE 
  | IF  
  | ELSE  
  | RETURN 

  | COLON

  type token = {
    tok_type: token_type;
    literal: string;
  }

  let token_to_string tok = 
    match tok.tok_type with
    | ILLEGAL -> "ILLEGAL: " ^ tok.literal
    | EOF -> "EOF"
    | IDENT -> "IDENT: " ^ tok.literal
    | INT -> "INT: " ^ tok.literal
    | STRING -> "INT: " ^ tok.literal
    | LBRACKET -> "["
    | RBRACKET -> "]"
    | ASSIGN -> "ASSIGN"
    | PLUS -> "+"
    | MINUS -> "-"
    | BANG -> "!"
    | ASTERISK -> "*"
    | SLASH -> "/"
    | LT -> "<"
    | GT -> ">"
    | EQ -> "=="
    | NOT_EQ -> "!="
    | COMMA -> ","
    | SEMICOLON -> ","
    | LPAREN -> "("
    | RPAREN -> "("
    | LBRACE -> "{"
    | RBRACE -> "}"
    | FUNCTION -> "FN"
    | LET -> "LET"
    | RETURN -> "RETURN"
    | IF -> "IF"
    | ELSE -> "ELSE"
    | TRUE -> "TRUE"
    | FALSE -> "FALSE"
    | COLON -> "COLON"
end 
