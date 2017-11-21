open Lexer
open Parser
open ASD
open Codegen

let lexbuf = Lexing.from_channel stdin
in try
  let token_stream = (Stream.of_list (Lexer.tokenize lexbuf))
  in let ast = Parser.program token_stream
  in let document = ir_of_ast ast
  in print_endline document
with
  Lexer.Unexpected_character e ->
  begin
    Printf.printf "Unexpected character: `%c' at position '%d' on line '%d'\n"
      e.character e.pos e.line;
    exit 1
  end


