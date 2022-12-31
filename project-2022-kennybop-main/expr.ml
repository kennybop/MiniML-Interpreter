(* 
                         CS 51 Final Project
                        MiniML -- Expressions
*)

(*......................................................................
  Abstract syntax of MiniML expressions 
 *)

type unop =
  | Negate
;;
    
type binop =
  | Plus
  | Minus
  | Times
  | Equals
  | LessThan
;;

type varid = string ;;
  
type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Bool of bool                         (* booleans *)
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
  | Var varid -> SS.singleton varid
  | Num _ | Bool _  | Raise | Unassigned -> SS.empty
  | Unop (_ , expr) -> free_vars expr
  | Binop (_ , expr1 , expr2) -> SS.union (free_vars expr1) 
                                            (free_vars expr2)
  | Conditional (expr1 , expr2 , expr3) -> SS.union (SS.union (free_vars expr1) 
                                                    (free_vars expr2))
                                                    (free_vars expr3)
  | Fun (varid , expr) ->  SS.diff (free_vars expr) (SS.add varid SS.empty)
  | Let (varid , expr1 , expr2) -> SS.union (SS.remove varid (free_vars expr2)) 
                                              (free_vars expr1)
  | Letrec (varid , expr1 , expr2) -> SS.union (SS.remove varid (free_vars expr2)) 
                                              (SS.remove varid (free_vars expr1))
  | App (expr1, expr2) -> SS.union (free_vars expr1) (free_vars expr2)
;;

(* new_varname () -- Returns a freshly minted `varid` constructed with
   a running counter a la `gensym`. Assumes no variable names use the
   prefix "var". (Otherwise, they might accidentally be the same as a
   generated variable name.) *)
let new_varname () : varid =
  let suffix = ref 0 in 
  let symbol = "var" ^ string_of_int !suffix in 
                suffix := !suffix + 1;
                symbol ;;

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
  let rec sub_this (exp: expr) : expr =
    match exp with
    | Var x -> if x = var_name then repl else exp
    | Num _ | Bool _  | Raise | Unassigned -> exp
    | Unop (op, arg) -> Unop(op, sub_this arg)
    | Binop (op, arg1, arg2) -> Binop(op, 
                                      sub_this arg1,
                                      sub_this arg2)
    | Conditional (arg1, arg2, arg3) -> Conditional (sub_this arg1,
                                                     sub_this arg2,
                                                     sub_this arg3)
    | Fun (x, arg) -> if x = var_name then exp
                      else
                        if SS.mem x (free_vars repl) then
                          let v2 = new_varname() in 
                          Fun (v2, sub_this (subst x (Var v2) exp))
                        else 
                          Fun (x, sub_this arg)
    | Let (x, def, body) ->
       if x = var_name then Let(x, sub_this def, body)
       else 
        if SS.mem x (free_vars repl) then
          let v2 = new_varname() in 
          Let (v2, sub_this def, sub_this (subst x (Var v2) exp))
        else
          Let (x, sub_this def, sub_this body) 
    | Letrec (x, def, body) ->
       if x = var_name then exp
       else 
        if SS.mem x (free_vars repl) then
          let v2 = new_varname() in 
          Letrec (v2, sub_this def, sub_this (subst x (Var v2) exp))
        else
          Letrec (x, sub_this def, sub_this body)
    | App (arg1, arg2) -> App(sub_this arg1, sub_this arg2)
  in 
    sub_this exp ;;
     
(*......................................................................
  String representations of expressions
 *)

let binop_to_cstring (op: binop) : string = 
  match op with
  | Plus -> " + "
  | Minus -> " - "
  | Times -> " * "
  | Equals -> " = "
  | LessThan -> " < "
;;   

let binop_to_astring (op: binop) : string = 
  match op with
  | Plus -> " Plus"
  | Minus -> " Minus"
  | Times -> " Times"
  | Equals -> " Equals"
  | LessThan -> " LessThan"

(* exp_to_concrete_string exp -- Returns a string representation of
   the concrete syntax of the expression `exp` *)

let rec exp_to_concrete_string (exp : expr) : string =
  match exp with
  | Var v -> v
  | Num p -> string_of_int p
  | Bool b -> if b then "True" else "False"
  | Unop (_op, ex) -> "(-" ^ exp_to_concrete_string ex ^ ")"
  | Binop (op, ex1, ex2) -> "(" ^ exp_to_concrete_string ex1
                            ^ binop_to_cstring op ^
                            exp_to_concrete_string ex2 ^ ")"
  | Conditional (ex1, ex2, ex3) -> "if " ^ exp_to_concrete_string ex1
                                    ^ " then " ^ exp_to_concrete_string ex2
                                    ^ " else " ^ exp_to_concrete_string ex3
  | Fun (v, ex) -> "fun " ^ v ^ " -> " ^ exp_to_concrete_string ex
  | Let (v, ex1, ex2) -> "let " ^ v ^ " = " ^
                          exp_to_concrete_string ex1 ^ " in " ^
                          exp_to_concrete_string ex2
  | Letrec (v, ex1, ex2) -> "let rec " ^ v ^ " = " ^
                          exp_to_concrete_string ex1 ^ " in " ^
                          exp_to_concrete_string ex2
  | Raise -> "Raise Excpetion"
  | Unassigned -> "Unassigned"
  | App (ex1, ex2) -> exp_to_concrete_string ex1 ^ " " ^
                          exp_to_concrete_string ex2
                              ;;
     
(* exp_to_abstract_string exp -- Return a string representation of the
   abstract syntax of the expression `exp` *)
let rec exp_to_abstract_string (exp : expr) : string =
  match exp with
  | Var v -> "Var " ^ v
  | Num p -> "Num " ^ string_of_int p
  | Bool b -> "Bool " ^ (if b then "True" else "False")
  | Unop (_op, ex) -> "Unop (Negate, " ^ exp_to_abstract_string ex ^ ")"
  | Binop (op, ex1, ex2) -> "(Binop, " ^ binop_to_astring op ^ ", " ^
                            exp_to_abstract_string ex1 ^ ", " ^
                            exp_to_abstract_string ex2 ^ ")"
  | Conditional (ex1, ex2, ex3) -> "Conditional (" ^ exp_to_abstract_string ex1
                                    ^ ", " ^ exp_to_abstract_string ex2
                                    ^ ", " ^ exp_to_abstract_string ex3 ^ ")"
  | Fun (v, ex) -> "Fun (" ^ v ^ ", " ^ exp_to_abstract_string ex ^ ")"
  | Let (v, ex1, ex2) -> "Let (" ^ v ^ ", " ^
                          exp_to_abstract_string ex1 ^ ", " ^
                          exp_to_abstract_string ex2 ^ ")"
  | Letrec (v, ex1, ex2) -> "Letrec (" ^ v ^ ", " ^
                          exp_to_abstract_string ex1 ^ ", " ^
                          exp_to_abstract_string ex2 ^ ")"
  | Raise -> "Raise Excpetion"
  | Unassigned -> "Unassigned"
  | App (ex1, ex2) -> "App (" ^ exp_to_abstract_string ex1 ^ ", " ^
                          exp_to_abstract_string ex2 ^ ")"
                              ;; 
