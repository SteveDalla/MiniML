(* 
                         CS 51 Final Project
                        MiniML -- Expressions
*)

(*......................................................................
  Abstract syntax of MiniML expressions 
 *)

type unop =
  | Negate
  | FloatNegate
  | Not
  | Sine 
  | Cosine
  | Tangent
  | NaturalLog
;;
    
type binop =
  | Plus
  | FloatPlus
  | Minus
  | FloatMinus
  | Times
  | FloatTimes
  | Divide
  | FloatDivide
  | Power
  | Equals
  | NotEquals
  | LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual
  | Concat
;;

type varid = string ;;
  
type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Float of float                       (* floats *)
  | Bool of bool                         (* booleans *)
  | String of string                     (* strings *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
;;
  
(*......................................................................
  Manipulation of variable names (varids) and sets of them
 *)

(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;

type varidset = SS.t ;;

(* same_vars varids1 varids2 -- Tests to see if two `varid` sets have
   the same elements (for testing purposes) *)
let same_vars : varidset -> varidset -> bool =
  SS.equal;;

(* vars_of_list varids -- Generates a set of variable names from a
   list of `varid`s (for testing purposes) *)
let vars_of_list : string list -> varidset =
  SS.of_list ;;
  
(* free_vars exp -- Returns the set of `varid`s corresponding to free
   variables in `exp` *)
let rec free_vars (exp : expr) : varidset =
  match exp with 
  | Num _ 
  | Float _
  | Bool _ 
  | String _
  | Raise 
  | Unassigned -> SS.empty
  | Var v -> SS.singleton v
  | Unop (_, e) -> free_vars e
  | Binop (_, e1, e2) -> SS.union (free_vars e1) (free_vars e2)
  | Conditional (e1, e2, e3) -> 
     SS.union (SS.union (free_vars e1) (free_vars e2)) (free_vars e3)
  | Fun (v, e) -> SS.remove v (free_vars e)
  | Let (v, e1, e2) -> SS.union (free_vars e1) (SS.remove v (free_vars e2))
  | Letrec (v, e1, e2) -> 
     SS.union (SS.remove v (free_vars e1)) (SS.remove v (free_vars e2))
  | App (e1, e2) -> SS.union (free_vars e1) (free_vars e2)
;;
  
(* new_varname () -- Returns a freshly minted `varid` constructed with
   a running counter a la `gensym`. Assumes no other variable names
   use the prefix "var". (Otherwise, they might accidentally be the
   same as a generated variable name.) *)

let gensym : string -> string =
   let counter = ref 0 in
   fun prefix -> let _ =  counter := !counter + 1 in 
                  prefix ^ string_of_int !counter
;;

let new_varname () : varid =
   gensym "var" ;;
;;

(*......................................................................
  Substitution 

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst var_name repl exp -- Return the expression `exp` with `repl`
   substituted for free occurrences of `var_name`, avoiding variable
   capture *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
   if not (SS.mem var_name (free_vars exp)) then exp else
   match exp with 
   | Num _
   | Float _
   | Bool _
   | String _
   | Raise
   | Unassigned -> exp
   | Var v -> if v = var_name then repl else exp
   | Unop (u, e) -> Unop (u, subst var_name repl e)
   | Binop (b, e1, e2) -> 
      Binop (b, subst var_name repl e1, subst var_name repl e2)
   | Conditional (e1, e2, e3) -> 
      Conditional (subst var_name repl e1, subst var_name repl e2, 
                   subst var_name repl e3)
   | Fun (v, e) -> 
      if v = var_name then exp 
      else if not (SS.mem v (free_vars repl)) then Fun (v, subst var_name repl e)
      else let new_var = new_varname () in 
           Fun (new_var, subst var_name repl (subst v (Var new_var) e))
   | Let (v, e1, e2) -> 
      if v = var_name then Let (v, subst var_name repl e1, e2)
      else if not (SS.mem v (free_vars repl)) then 
         Let (v, subst var_name repl e1, subst var_name repl e2)
      else let new_var = new_varname () in 
           Let (new_var, subst var_name repl e1, 
                subst var_name repl (subst v (Var new_var) e2))
   | Letrec (v, e1, e2) -> 
      if v = var_name then Letrec (v, subst var_name repl e1, e2)
      else if not (SS.mem v (free_vars repl)) then 
         Letrec (v, subst var_name repl e1, subst var_name repl e2)
      else let new_var = new_varname () in 
           Letrec (new_var, subst var_name repl e1, 
                   subst var_name repl (subst v (Var new_var) e2))
   | App (e1, e2) -> App (subst var_name repl e1, subst var_name repl e2)
;;
     
(*......................................................................
  String representations of expressions
 *)
   
(* exp_to_concrete_string exp -- Returns a string representation of
   the concrete syntax of the expression `exp` *)

let unop_string (syntax : expr -> string) (u : unop) (e : expr) : string =
  let e' = syntax e in 
  match u with
  | Negate             -> "~-" ^ e'
  | FloatNegate        -> "~-." ^ e'
  | Not                -> "not " ^ e'
  | Sine               -> "sin " ^ e'
  | Cosine             -> "cos " ^ e'
  | Tangent            -> "tan " ^ e'
  | NaturalLog         -> "natural log " ^ e'

let binop_string (syntax : expr -> string) (b : binop) (e1 : expr) (e2 : expr) : string =
  let e1' = syntax e1 in 
  let e2' = syntax e2 in 
  match b with 
  | Plus               -> e1' ^ " + " ^ e2'
  | FloatPlus          -> e1' ^ " +. " ^ e2'
  | Minus              -> e1' ^ " - " ^ e2'
  | FloatMinus         -> e1' ^ " -. " ^ e2'
  | Times              -> e1' ^ " * " ^ e2'
  | FloatTimes         -> e1' ^ " *. " ^ e2'
  | Divide             -> e1' ^ " / " ^ e2'
  | FloatDivide        -> e1' ^ " /. " ^ e2'
  | Power              -> e1' ^ " ** " ^ e2'
  | Equals             -> e1' ^ " = " ^ e2'
  | NotEquals          -> e1' ^ " <> " ^ e2'
  | LessThan           -> e1' ^ " < " ^ e2'
  | LessThanOrEqual    -> e1' ^ " <= " ^ e2'
  | GreaterThan        -> e1' ^ " > " ^ e2'
  | GreaterThanOrEqual -> e1' ^ " >= " ^ e2'
  | Concat             -> e1' ^ " ^ " ^ e2'

let rec exp_to_concrete_string (exp : expr) : string =
  match exp with 
  | Var v  -> v
  | Num n  -> string_of_int n
  | Float f -> string_of_float f
  | Bool b -> string_of_bool b
  | String s -> "\"" ^ s ^ "\""
  | Unop (u, e) -> unop_string exp_to_concrete_string u e
  | Binop (b, e1, e2) -> binop_string exp_to_concrete_string b e1 e2
  | Conditional (e1, e2, e3) ->
     let e1_string = exp_to_concrete_string e1 in 
     let e2_string = exp_to_concrete_string e2 in 
     let e3_string = exp_to_concrete_string e3 in
     "if " ^ e1_string ^ " then " ^ e2_string ^ " else " ^ e3_string
  | Fun (v, e) -> 
     let e_string = exp_to_concrete_string e in 
     "fun " ^ v ^ " -> " ^ e_string
  | Let (v, e1, e2) -> 
     let e1_string = exp_to_concrete_string e1 in 
     let e2_string = exp_to_concrete_string e2 in 
     "let " ^ v ^ " = " ^ e1_string ^ " in " ^ e2_string
  | Letrec (v, e1, e2) ->
     let e1_string = exp_to_concrete_string e1 in 
     let e2_string = exp_to_concrete_string e2 in 
     "let rec " ^ v ^ " = " ^ e1_string ^ " in " ^ e2_string
  | Raise -> "error"
  | Unassigned -> "unassigned"
  | App (e1, e2) ->
     let e1_string = exp_to_concrete_string e1 in 
     let e2_string = exp_to_concrete_string e2 in 
     e1_string ^ " (" ^ e2_string ^ ")"
;;

(* exp_to_abstract_string exp -- Return a string representation of the
   abstract syntax of the expression `exp` *)
let rec exp_to_abstract_string (exp : expr) : string =
  match exp with 
  | Var v  -> "Var(" ^ v ^ ")"
  | Num n  -> "Num(" ^ string_of_int n ^ ")"
  | Float f -> "Float(" ^ string_of_float f ^ ")"
  | Bool b -> "Bool(" ^ string_of_bool b ^ ")"
  | String s -> "String(\"" ^ s ^ "\")"
  | Unop (u, e) -> 
     let u_string = 
      match u with 
      | Negate             -> "Negate"
      | FloatNegate        -> "FloatNegate"
      | Not                -> "Not"
      | Sine               -> "Sine"
      | Cosine             -> "Cosine"
      | Tangent            -> "Tangent"
      | NaturalLog         -> "NaturalLog" in
     "Unop(" ^ u_string ^ ", " ^ exp_to_abstract_string e ^ ")"
  | Binop (b, e1, e2) ->
     let b_string = 
       match b with 
       | Plus               -> "Plus"
       | FloatPlus          -> "FloatPlus"
       | Minus              -> "Minus"
       | FloatMinus         -> "FloatMinus"
       | Times              -> "Times" 
       | FloatTimes         -> "FloatTimes"  
       | Divide             -> "Divide" 
       | FloatDivide        -> "FloatDivide" 
       | Power              -> "Power" 
       | Equals             -> "Equals" 
       | NotEquals          -> "NotEquals" 
       | LessThan           -> "LessThan" 
       | LessThanOrEqual    -> "LessThanOrEqual" 
       | GreaterThan        -> "GreaterThan"
       | GreaterThanOrEqual -> "GreaterThanOrEqual" 
       | Concat             -> "Concat" in 
     "Binop(" ^ b_string ^ ", " ^ exp_to_abstract_string e1 ^ ", " ^
                                  exp_to_abstract_string e2 ^ ")"
  | Conditional (e1, e2, e3) ->
     let e1_string = exp_to_abstract_string e1 in 
     let e2_string = exp_to_abstract_string e2 in 
     let e3_string = exp_to_abstract_string e3 in
     "Conditional(" ^ e1_string ^ ", " ^ e2_string ^ ", " ^ e3_string ^ ")"
  | Fun (v, e) -> 
     let e_string = exp_to_abstract_string e in 
     "Fun(" ^ v ^ ", " ^ e_string ^ ")"
  | Let (v, e1, e2) -> 
     let e1_string = exp_to_abstract_string e1 in 
     let e2_string = exp_to_abstract_string e2 in 
     "Let(" ^ v ^ ", " ^ e1_string ^ ", " ^ e2_string ^ ")"
  | Letrec (v, e1, e2) ->
     let e1_string = exp_to_abstract_string e1 in 
     let e2_string = exp_to_abstract_string e2 in 
     "Letrec(" ^ v ^ ", " ^ e1_string ^ ", " ^ e2_string ^ ")"
  | Raise -> "parse error"
  | Unassigned -> "unassigned"
  | App (e1, e2) ->
     let e1_string = exp_to_abstract_string e1 in 
     let e2_string = exp_to_abstract_string e2 in 
     "App(" ^ e1_string ^ ", " ^ e2_string ^ ")"
;;
