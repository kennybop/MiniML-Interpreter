(* 
                         CS 51 Final Project
                         MiniML -- Evaluation
*)

(* This module implements a small untyped ML-like language under
   various operational semantics.
 *)

open Expr ;;
  
(* Exception for evaluator runtime, generated by a runtime error in
   the interpreter *)
exception EvalError of string ;;
  
(* Exception for evaluator runtime, generated by an explicit `raise`
   construct in the object language *)
exception EvalException ;;

(*......................................................................
  Environments and values 
 *)

module type ENV = sig
    (* the type of environments *)
    type env
    (* the type of values stored in environments *)
    type value =
      | Val of expr
      | Closure of (expr * env)
   
    (* empty () -- Returns an empty environment *)
    val empty : unit -> env

    (* close expr env -- Returns a closure for `expr` and its `env` *)
    val close : expr -> env -> value

    (* lookup env varid -- Returns the value in the `env` for the
       `varid`, raising an `Eval_error` if not found *)
    val lookup : env -> varid -> value

    (* extend env varid loc -- Returns a new environment just like
       `env` except that it maps the variable `varid` to the `value`
       stored at `loc`. This allows later changing the value, an
       ability used in the evaluation of `letrec`. To make good on
       this, extending an environment needs to preserve the previous
       bindings in a physical, not just structural, way. *)
    val extend : env -> varid -> value ref -> env

    (* env_to_string env -- Returns a printable string representation
       of environment `env` *)
    val env_to_string : env -> string
                                 
    (* value_to_string ?printenvp value -- Returns a printable string
       representation of a value; the optional flag `printenvp`
       (default: `true`) determines whether to include the environment
       in the string representation when called on a closure *)
    val value_to_string : ?printenvp:bool -> value -> string
  end

module Env : ENV =
  struct
    type env = (varid * value ref) list
     and value =
       | Val of expr
       | Closure of (expr * env)

    let empty () : env = []

    let close (exp : expr) (env : env) : value =
      Closure (exp, env) ;;

    let lookup (env : env) (varname : varid) : value =
      try !(List.assoc varname env)
      with | Not_found -> raise (EvalError "not found");;
  
    let extend (env : env) (varname : varid) (loc : value ref) : env =
      try 
        let _q = lookup env varname in 
        List.map (fun (a, b) -> if a = varname then (a, loc)
                                else (a,b)) env
      with
      | EvalError _ -> (varname, loc) :: env ;;

    let rec value_to_string ?(printenvp : bool = true) (v : value) : string =
      match v with
      | Val expr -> exp_to_concrete_string expr
      | Closure (expr, env) ->
          if printenvp then "(" ^ exp_to_concrete_string expr
                            ^ ", " ^ env_to_string env
          else exp_to_concrete_string expr

    and

     env_to_string (env : env) : string =
      List.fold_left (^) ""
      (List.map 
        (fun (a,b) -> "(" ^ a ^ ", " ^ value_to_string !b ^ ")") env) ;;

  end
;;


(*......................................................................
  Evaluation functions

  Each of the evaluation functions below evaluates an expression `exp`
  in an environment `env` returning a result of type `value`. We've
  provided an initial implementation for a trivial evaluator, which
  just converts the expression unchanged to a `value` and returns it,
  along with "stub code" for three more evaluators: a substitution
  model evaluator and dynamic and lexical environment model versions.

  Each evaluator is of type `expr -> Env.env -> Env.value` for
  consistency, though some of the evaluators don't need an
  environment, and some will only return values that are "bare
  values" (that is, not closures). 

  DO NOT CHANGE THE TYPE SIGNATURES OF THESE FUNCTIONS. Compilation
  against our unit tests relies on their having these signatures. If
  you want to implement an extension whose evaluator has a different
  signature, implement it as `eval_e` below.  *)

(* The TRIVIAL EVALUATOR, which leaves the expression to be evaluated
   essentially unchanged, just converted to a value for consistency
   with the signature of the evaluators. *)
   
let eval_t (exp : expr) (_env : Env.env) : Env.value =
  (* coerce the expr, unchanged, into a value *)
  Env.Val exp ;;

(* The SUBSTITUTION MODEL evaluator -- to be completed *)

(* Helper functions for unary and binary evaluations to
   make code look more clean *)

let unary_help (op: unop) (x: Env.value) : Env.value =
  match op, x with
  | Negate, Env.Val (Num n) ->  Env.Val (Num (~-n))
  | Negate, _ -> raise (EvalError "Unop must take int")

let binary_help (op: binop) (x1: Env.value)
                            (x2: Env.value) : Env.value =
    match op, x1, x2 with
    | Plus, Env.Val (Num v1), Env.Val (Num v2) -> Env.Val (Num (v1 + v2))
    | Minus, Env.Val (Num v1), Env.Val (Num v2) -> Env.Val (Num (v1 - v2))
    | Times, Env.Val (Num v1), Env.Val (Num v2) -> Env.Val (Num (v1 * v2))
    | Equals, Env.Val (Num v1), Env.Val (Num v2) -> Env.Val (Bool (v1 = v2))
    | LessThan, Env.Val (Num v1), Env.Val (Num v2) -> Env.Val (Bool (v1 < v2))

    | Plus, _, _ 
    | Minus, _, _
    | Times, _, _
    | Equals, _, _
    | LessThan, _, _ -> raise (EvalError "Binop must take int")

(* Helper function to get the expression out of the value returned
   from the evaluator function *)

let get_expr (ex: Env.value) : expr =
  match ex with
  | Env.Val x -> x
  | Env.Closure (x, _e) -> x ;;
   
let eval_s (exp : expr) (_env : Env.env) : Env.value =
  let rec evaluator (exp: expr) : Env.value =
    match exp with
    | Var x -> raise (EvalError ("Unbound variable " ^ x))
    | Num _ | Bool _ | Fun _ -> Env.Val exp 
    | Unassigned -> raise (EvalError "Unassinged")
    | Unop (op, ex) -> unary_help op (evaluator ex)
    | Binop (op, ex1, ex2) -> binary_help op (evaluator ex1)
                                             (evaluator ex2)
    | Conditional (ex1, ex2, ex3) -> 
        if (evaluator ex1 = Env.Val (Bool true)) 
        then evaluator ex2
        else evaluator ex3
    | Let (v, def, body) -> 
        evaluator (subst v (get_expr (evaluator def)) body)
    | Letrec (v, def, body) -> 
        let new_exp = get_expr 
                (evaluator (subst v (Letrec (v, def, Var v)) def))
        in evaluator(subst v new_exp body)
    | Raise -> raise (EvalException)
    | App (ex1, ex2) -> 
        match evaluator ex1 with
        | Env.Val (Fun (x, def)) -> 
            evaluator (subst x (get_expr (evaluator ex2)) def)
        | _ -> raise (EvalError "Can only apply functions")
  in evaluator exp ;;


(* default evaluator which can be used for lex or dyn in order to
   avoid the repetition in the two types of scope *)
 
type scope = | Lexical | Dynamic 

let defualt_eval (s: scope) (exp: expr) (env: Env.env) : Env.value =
  let rec evaluator (exp: expr) (env: Env.env) : Env.value =
    match exp with
    | Var v -> (try 
               match Env.lookup env v with
               | Env.Val v -> Env.Val v
               | Env.Closure (v, xenv) -> evaluator v xenv 
               with
               | Not_found -> raise (EvalError ("Unbound variable " ^ v)))
    | Num _ | Bool _  -> Env.Val exp 
    | Fun _ -> if s = Lexical then Env.close exp env else Env.Val exp
    | Unassigned -> raise (EvalError "Unassinged")
    | Unop (op, ex) -> unary_help op (evaluator ex env)
    | Binop (op, ex1, ex2) -> binary_help op (evaluator ex1 env)
                                             (evaluator ex2 env)
    | Conditional (ex1, ex2, ex3) -> 
        if (evaluator ex1 env = Env.Val (Bool true)) 
        then evaluator ex2 env
        else evaluator ex3 env
    | Let (v, def, body) -> 
            let xenv = Env.extend env v (ref (evaluator def env))
            in evaluator body xenv
    | Letrec (v, def, body) -> 
            let xv = ref (Env.Val Unassigned) in
            let xenv = Env.extend env v xv in 
            let xdef = evaluator def xenv in 
              xv := xdef ;
              evaluator body (Env.extend xenv v (ref xdef))
    | Raise -> raise (EvalException)
    | App (ex1, ex2) -> 
      if s = Lexical then (
        match evaluator ex1 env with
        | Env.Closure (Fun (x, def), xenv) -> 
              evaluator def (Env.extend xenv x (ref (evaluator ex2 env)))
        | _ -> raise (EvalError "Can only apply functions"))
      else (
        match evaluator ex1 env with 
        | Env.Val (Fun (a, b)) -> 
            evaluator b (Env.extend env a (ref (evaluator ex2 env)))
        | _ -> raise (EvalError "Can only apply functions"))
    in evaluator exp env ;;


(* The DYNAMICALLY-SCOPED ENVIRONMENT MODEL evaluator -- to be
   completed *)
   
let eval_d (exp : expr) (env : Env.env) : Env.value =
  defualt_eval Dynamic exp env ;;
 
       
(* The LEXICALLY-SCOPED ENVIRONMENT MODEL evaluator -- optionally
   completed as (part of) your extension *)
   
let eval_l (exp : expr) (env : Env.env) : Env.value =
  defualt_eval Lexical exp env ;;

  
(* Connecting the evaluators to the external world. The REPL in
   `miniml.ml` uses a call to the single function `evaluate` defined
   here. Initially, `evaluate` is the trivial evaluator `eval_t`. But
   you can define it to use any of the other evaluators as you proceed
   to implement them. (We will directly unit test the four evaluators
   above, not the `evaluate` function, so it doesn't matter how it's
   set when you submit your solution.) *)
   
let evaluate1 = eval_s ;;
let evaluate2 = eval_d ;;
let evaluate3 = eval_l ;;
