module Lexer = struct
type lexer = {
    input: string;
    currPosition: int;
    peekPosition: int;
    ch: char
};;

let null_byte = '\x00'

let is_alpha ch = 
    'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch = '_';;
let is_digit ch = 
    '0' <= ch && ch <= '9';;

let rc lex = 
    if lex.currPosition >= String.length lex.input
        then null_byte
        else String.get lex.input lex.currPosition

let read_char lex = 
    {
        lex with currPosition=lex.currPosition;
        peekPosition=lex.currPosition + 1;
        ch = rc lex
    }
let spawn_lexer raw_text =
    let lex = {
        input=raw_text;
        currPosition=0;
        peekPosition=0;
        ch=null_byte
    } in read_char lex

end