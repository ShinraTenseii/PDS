open ASD
open Llvm
open Utils
open List
open SymbolTable

(* main function. return only a string: the generated code *)
let rec ir_of_ast p =
  (* this header describe to LLVM the target
   * and declare the external function printf
   *)
  let head = "; Target\n" ^
  "target triple = \"x86_64-unknown-linux-gnu\"\n" ^
  "; External declaration of the printf function\n" ^
  "declare i32 @printf(i8* noalias nocapture, ...)\n" ^
  "\n; Actual code begins\n\n"


(* TODO : change when you extend the language *)
  in let (ir, _) = ir_of_instruction (empty_ir, []) p
  (* adds the return instruction *)
  in let new_ir = {
    header = ir.header;
    code = ir.code
  }
  (* generates the final string *)
  in head ^

  (* We create the function main *)
  (* TODO : remove this when you extend the language *)
  "define i32 @main() {\n" ^

     (string_of_ir new_ir)^
  (* TODO : remove this when you extend the language *)
  (* TODO : remove this when you extend the language *)
  "}\n"

and llvm_type_of_asd_typ = function
  | Type_Int -> LLVM_Type_Int

(* All main code generation functions take the current IR and a scope,
 * append header and/or code to the IR, and/or change the scope
 * They return the new pair (ir, scope)
 * This is convenient with List.fold_left
 *
 * They can return other stuff (synthesized attributes)
 * They can take extra arguments (inherited attributes)
 *)


(* returns the regular pair, plus the pair of the name of the result and its type *)
and ir_of_expression (ir, scope) =
  (* function to generate all binop operations *)
  let aux op t (l, r) =
    (* generation of left operand computation. We give directly (ir, scope) *)
    let ll, (lresult_name, lresult_type) = ir_of_expression (ir, scope) l
    (* generation of right operand computation. We give directly (ir, scope) from the left computation *)
    (* it appends code to the previous code generated *)
    in let rr, (rresult_name, rresult_type) = ir_of_expression ll r

    (* allocate a new unique locale identifier *)
    and result = newtmp ()

    (* type checking *)
    in let _ = if lresult_type <> rresult_type || t <> rresult_type then failwith "Type error"

    (* new instruction *)
    in let code = Binop {
      lvalue_name = result;
      lvalue_type = t;
      op = op;
      left = lresult_name;
      right = rresult_name;
    }

    (* Returns : *)
    in (({
      header = (fst rr).header;
      code = code :: (fst rr).code;
    }, scope), (result, t))

  in function
    | AddExpression (l, r)  -> aux "add" LLVM_Type_Int (l, r) (* For now, all binop are integer *)
    | SubExpression (l, r)  -> aux "sub" LLVM_Type_Int (l, r)
    | MulExpression (l, r)  -> aux "mul" LLVM_Type_Int (l, r)
    | DivExpression (l, r)  -> aux "sdiv" LLVM_Type_Int (l, r)
    | IntegerExpression i    -> ((ir, scope), (string_of_int i, LLVM_Type_Int))

and  ir_of_affectation (ir, scope) =

	let aux op t p (var, expr) =

    let ((irEx, _), (retEx, _)) = ir_of_expression (empty_ir, scope) expr

	in let resultVar = affectVar var

    (*in let _ = if t <>  then failwith "Type error"*)

	in let codeAff = Affect {
	  assign = op;
      value_type = t;
      valueVar = retEx;
      pointer = p;
	  nameVar = resultVar;
   }

   in ({
      header = irEx.header;
      code = codeAff :: irEx.code;
    }, scope)

   in function
    | AffectInstr (v, e) -> aux "store" LLVM_Type_Int LLVM_Type_Pointer (v, e)
   (* | IdentVar s -> ((ir, scope), (valueVar, LLVM_Type_Int))*)
  (* in function
    | IdentVar (t,s) -> aux "store" LLVM_Type_Int t LLVM_Type_Pointer s *)
and ir_of_instruction (ir, scope) = function
  | AffectInstr(var, exp) -> ir_of_affectation(ir, scope)
  | DeclarInstr(var) -> ir_of_declaration (ir, scope)

and ir_of_declaration (ir, scope) =

  let aux var eq op typ =

  and let resultVar = affectVar var

  in let codeDec = DeclVar {
    var_name=resultVar;
    equal=eq;
    allocate=op;
    value_type=typ;
    }

  in ({
    header = ir.header;
    code = codeDec :: ir.code;
  }, scope)

  in function
    | DeclarInstr v -> aux v "=" "alloca" LLVM_Type_Int

and let aux var eq op typ typ2 n =

  in let resultVar = affectVar var

  in codeDec = DeclTab {
    var_name=resultVar;
    equal=eq;
    allocate=op;
    tab_value_type=typ;
    value_type=typ2;
    value=n;
  }

in ({
    header = ir.header;
    code = codeDec :: ir.code;
  }, scope)

  in function
    | DeclarInstr (v, n) -> aux2 v "=" "alloca" LLVM_Type_Int, LLVM_Type_Int n
