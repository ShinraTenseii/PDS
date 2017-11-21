(* TODO : extend when you extend the language *)

type ident = string

type expression =
  | AddExpression of expression * expression
  | SubExpression of expression * expression
  | MulExpression of expression * expression
  | DivExpression of expression * expression
  | IntegerExpression of int

type variable = 
  | IdentVar of string

type instruction = 
  | AffectInstr of variable * expression 

type typ =
  | Type_Int

type program = expression
