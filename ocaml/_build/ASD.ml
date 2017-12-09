(* TODO : extend when you extend the language *)

type ident = string

type expression =
  | AddExpression of expression * expression
  | SubExpression of expression * expression
  | MulExpression of expression * expression
  | DivExpression of expression * expression
  | IntegerExpression of int
  | IdentExprVar of string
  | IdentExprTab of string * int

type variable =
  | IdentVar of string
  | IdentTab of string * int

type instruction =
  | AffectInstr of variable * expression
  | DeclarInstr of variable

type typ =
  | Type_Int

  type sequence =
    | IdentIF of string
    | IdentFI of string
    | IdentDO of string
    | IdentOD of string
    | IdentWH of string
    | IdentTH of string
    | IdentEL of string

type program = expression
