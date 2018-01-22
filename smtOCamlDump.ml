(* SmtOCamlDump:  a tiny and incomplete API to dump into the Smtlib format *)
(* Copyright (C) 2016-2018, Laure Gonnord, Julien Braine, David Monniaux *)
(*    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program. If not, see <https://www.gnu.org/licenses/ *)

(** some utilitary functions **)
let string_equals s1 s2 = (String.compare s1 s2)==0
(*Does a union of the elements of both list. Allows lists to be used as Sets*)
let list_union l1 l2 =
  List.fold_left (fun l (str,t) -> if List.exists (fun (s, ty) -> string_equals s str) l then l else (str,t)::l) l1 l2
    

(** Types and constructors **)

(*abstract types*)
type abstract_type = 
  | Tr_int
  | Tr_bool
  | Tr_array of abstract_type
  
	 
(* Operators. *)
(* Unary operators. *)
type fe_unary_op =
  | Fe_neg
  | Fe_not

let feop2string = function
  | Fe_neg -> "-"
  | Fe_not -> "!"
    
(* Binary operators. *)
type fe_binary_op =
  | Fe_add | Fe_sub | Fe_mul | Fe_div | Fe_mod
  | Fe_or | Fe_and | Fe_xor
  | Fe_lt | Fe_le | Fe_gt | Fe_ge
  | Fe_eq
      
let bop2string = function
  | Fe_add -> "+"
  | Fe_sub -> "-"
  | Fe_mul -> "*"
  | Fe_div -> "/"
  | Fe_mod -> "%"
  | Fe_or -> "||"
  | Fe_and -> "&&"
  | Fe_xor -> "^"
  | Fe_lt -> "<"
  | Fe_le -> "<="
  | Fe_gt -> ">"
  | Fe_ge -> ">="
  | Fe_eq -> "=="

type f_var = string (*can be anything*)

let f_var2string = fun x-> x
  
(*abstract variable*)
type abstract_var =
{ 
  trans_initial_var : f_var;
  trans_type : abstract_type;
}
type s_command = string
  
(*Abstract commands*)
type abstract_command =
| Tr_start of abstract_var list
| Tr_end of abstract_var list
| Tr_command of s_command * (abstract_var list) (*the abstract var list is the list of variables used by the command*)


(*Abstract expression*)
type abstract_expr =
| Fe_variable of abstract_var
| Fe_bool of bool
| Fe_int of int64
| Fe_unary of fe_unary_op * abstract_expr
| Fe_binary of fe_binary_op * abstract_expr * abstract_expr
| Fe_read of abstract_type (*Replaces ?r in old version*)
| Fe_random
| Fe_string_var of string
| Fe_tuple of abstract_expr list
| Fe_select of abstract_expr * abstract_expr
| Fe_store of abstract_expr * abstract_expr * abstract_expr
| Fe_insert of abstract_expr * abstract_expr * abstract_expr
| Fe_remove of abstract_expr * abstract_expr
| Fe_size of abstract_expr
| Fe_emptyarray of abstract_expr

(*A smt2 formula*)
type formula = 
(*A predicate in a formula is a command where each variable is replaced by an expression. 
  For example, start i j => affect i (i+1) will be translated by
  F_implies(F_node(start, fun v -> v), F_node(affect, fun v -> if (v == j) then (i+1) else v))*)
| F_node of abstract_command*(abstract_var -> abstract_expr) 
| F_implies of formula*formula
| F_and of formula list
| F_bformula of abstract_expr
| F_comment of string



(** pretty print and print in file **)
(*Print a given expression (in string)*)
let rec print_expr expr =
  match expr with
  | Fe_variable(v) ->
     begin match v.trans_type with
     | Tr_int -> (f_var2string v.trans_initial_var)^ " "
     | Tr_bool -> (f_var2string v.trans_initial_var)^ " "
     | Tr_array(t) -> (f_var2string v.trans_initial_var)^ " "
     end
  | Fe_string_var(s) -> s
  | Fe_bool(b) -> Printf.sprintf "%B" b
  | Fe_int(i) -> Printf.sprintf "%Ld" i
  | Fe_unary(Fe_neg, exp) -> "(- "^(print_expr exp)^")"
  | Fe_unary(Fe_not, exp) -> "(not "^(print_expr exp)^")"
  | Fe_binary(op, Fe_variable(v), e) ->
     begin match v.trans_type with
     | _ -> print_expr (Fe_binary(op, Fe_string_var(print_expr (Fe_variable(v))), e))
     end
  | Fe_binary(op, e, Fe_variable(v)) ->
     begin match v.trans_type with
     | _ -> print_expr (Fe_binary(op, e, Fe_string_var(print_expr (Fe_variable(v)))))
     end
  | Fe_binary(Fe_add, exp1, exp2) -> "(+ "^(print_expr exp1)^" "^(print_expr exp2)^")"
  | Fe_binary(Fe_sub, exp1, exp2) -> "(- "^(print_expr exp1)^" "^(print_expr exp2)^")"
  | Fe_binary(Fe_mul, exp1, exp2) -> "(* "^(print_expr exp1)^" "^(print_expr exp2)^")"
  | Fe_binary(Fe_div, exp1, exp2) -> "(/ "^(print_expr exp1)^" "^(print_expr exp2)^")"
  | Fe_binary(Fe_mod, exp1, exp2) -> "(mod "^(print_expr exp1)^" "^(print_expr exp2)^")"
  | Fe_binary(Fe_or, exp1, exp2) -> "(or "^(print_expr exp1)^" "^(print_expr exp2)^")"
  | Fe_binary(Fe_and, exp1, exp2) -> "(and "^(print_expr exp1)^" "^(print_expr exp2)^")"
  | Fe_binary(Fe_xor, exp1, exp2) -> "(xor "^(print_expr exp1)^" "^(print_expr exp2)^")"
  | Fe_binary(Fe_lt, Fe_tuple(l1), Fe_tuple(l2)) ->
     begin match (l1,l2) with
     | [], [] -> ""
     | [a], [b] -> print_expr (Fe_binary(Fe_lt, a, b))
     | a::q, b::t -> print_expr (Fe_binary(Fe_or, Fe_binary(Fe_lt, a, b), 
                                           Fe_binary(Fe_and, Fe_binary(Fe_eq, a, b), 
                                                     Fe_binary(Fe_lt, Fe_tuple(q), Fe_tuple(t))))) 
     | _ -> failwith "Tuples do not have same size"
     end
  | Fe_binary(Fe_lt, exp1, exp2) -> "(< "^(print_expr exp1)^" "^(print_expr exp2)^")"
  | Fe_binary(Fe_le, exp1, exp2) -> "(<= "^(print_expr exp1)^" "^(print_expr exp2)^")"
  | Fe_binary(Fe_gt, exp1, exp2) -> "(> "^(print_expr exp1)^" "^(print_expr exp2)^")"
  | Fe_binary(Fe_ge, exp1, exp2) -> "(>= "^(print_expr exp1)^" "^(print_expr exp2)^")"
  | Fe_binary(Fe_eq, Fe_tuple(l1), Fe_tuple(l2)) ->
     begin match (l1,l2) with
     | [], [] -> ""
     | [a], [b] -> print_expr (Fe_binary(Fe_eq, a, b))
     | a::q, b::t -> print_expr (Fe_binary(Fe_and, Fe_binary(Fe_eq, a, b),
                                           Fe_binary(Fe_eq, Fe_tuple(q), Fe_tuple(t)))) 
     | _ -> failwith "Tuples do not have same size" end
  | Fe_binary(Fe_eq, exp1, exp2) -> "(= "^(print_expr exp1)^" "^(print_expr exp2)^")"
  | Fe_read(t) -> "?r"
  | Fe_random -> "rnd"
  | Fe_tuple(l) ->  List.fold_left (fun bef e -> bef^" "^(print_expr e)) "" l 
  | Fe_select(tab, index) -> Printf.sprintf "(select %s %s)" (print_expr tab) (print_expr index)
  | Fe_store(tab, index, value) -> Printf.sprintf "(store %s %s %s)" (print_expr tab) (print_expr index) (print_expr value)
  | Fe_insert(tab, index, value) -> Printf.sprintf "(insert %s %s %s)" (print_expr tab) (print_expr index) (print_expr value)
  | Fe_remove(tab, index) -> Printf.sprintf "(remove %s %s)" (print_expr tab) (print_expr index)
  | Fe_size(tab) -> Printf.sprintf "(size %s)" (print_expr tab)
  | Fe_emptyarray(tab) -> Printf.sprintf "(clear %s)" (print_expr tab)


(*Prints type for z3*)
let rec print_type t =
  match t with
  | Tr_int -> "Int"
  | Tr_bool -> "Bool"
  | Tr_array(v) -> Printf.sprintf "(Array Int %s)" (print_type v)


(*Get variables used in an expression and their types*)
let rec get_expr_variables e t =
  match e with
  | Fe_variable(v) -> begin match v.trans_type with
                      | Tr_int -> [(v.trans_initial_var, Tr_int)]
                      | Tr_bool -> [(v.trans_initial_var, Tr_bool)]
                      (*| Tr_value(ty) -> [(Printf.sprintf "%sd%iv"  v.trans_initial_var.s_var_name v.distinct_num, ty)]
                      | Tr_position(positions) -> List.mapi (fun i p -> (print_pos_v v i, p)) positions*)
                      | Tr_array(t)-> [(v.trans_initial_var, Tr_array(t))] end
  | Fe_string_var(s) -> [(s, t)]
  | Fe_bool(b) -> []
  | Fe_int(i) -> []
  | Fe_unary(_, exp) -> get_expr_variables exp t
  | Fe_binary(Fe_add, exp1, exp2) 
  | Fe_binary(Fe_sub, exp1, exp2) 
  | Fe_binary(Fe_mul, exp1, exp2) 
  | Fe_binary(Fe_div, exp1, exp2) 
  | Fe_binary(Fe_mod, exp1, exp2) 
  | Fe_binary(Fe_lt, exp1, exp2) 
  | Fe_binary(Fe_le, exp1, exp2) 
  | Fe_binary(Fe_gt, exp1, exp2) 
  | Fe_binary(Fe_ge, exp1, exp2) 
  | Fe_binary(Fe_eq, exp1, exp2) -> list_union (get_expr_variables exp1 Tr_int) (get_expr_variables exp2 Tr_int)
  | Fe_binary(Fe_and, exp1, exp2) 
  | Fe_binary(Fe_or, exp1, exp2) 
  | Fe_binary(Fe_xor, exp1, exp2) -> list_union (get_expr_variables exp1 Tr_int) (get_expr_variables exp2 Tr_bool)
  | Fe_read(ty) -> [("?r", ty)]
  | Fe_random -> [("rnd", t)]
  | Fe_tuple(l) ->  begin match t with
                    (*| Tr_position(tl) -> List.fold_left2 (fun s exp te -> list_union s (get_expr_variables exp te)) [] l tl*)
                    | _ -> List.fold_left (fun s exp -> list_union s (get_expr_variables exp t)) [] l end
  | Fe_select(tab, l) -> (get_expr_variables tab (Tr_array(Tr_int)) ) @ (get_expr_variables l Tr_int)
  | Fe_store(tab, l, e) -> (get_expr_variables tab (Tr_array(Tr_int))) @ (get_expr_variables l Tr_int)@(get_expr_variables e Tr_int)
  | Fe_insert(tab, index, value) -> (get_expr_variables tab (Tr_array(Tr_int))) @ (get_expr_variables index Tr_int)@(get_expr_variables value Tr_int)
  | Fe_remove(tab, index) -> (get_expr_variables tab (Tr_array(Tr_int))) @ (get_expr_variables index Tr_int)
  | Fe_size(tab) -> (get_expr_variables tab (Tr_array(Tr_int)))
  | Fe_emptyarray(tab) -> (get_expr_variables tab (Tr_array(Tr_int)))
     
(*Get variable used in a formula*)
let rec get_used_variables f = 
  match f with
  | F_node(Tr_command(command, vars), func) -> List.fold_left (fun set v -> list_union set (get_expr_variables (func v) (v.trans_type))) [] vars
  | F_node(Tr_start(vars), func) ->  List.fold_left (fun set v -> list_union set (get_expr_variables (func v) (v.trans_type))) [] vars
  | F_node(Tr_end(vars), func) -> List.fold_left (fun set v -> list_union set (get_expr_variables (func v) (v.trans_type))) [] vars
  | F_implies(f1, f2) -> list_union (get_used_variables f1) (get_used_variables f2) 
  | F_and(l) -> List.fold_left (fun bef f -> list_union bef (get_used_variables f)) [] l
  | F_bformula(e) -> (get_expr_variables e Tr_bool)
  | F_comment(s) -> failwith "comments not possible here"

(*Prints a formula*)


let rec print_formula_simple f = match f with
  | F_node(Tr_command(command, vars), func) -> "("^(command)^(List.fold_left (fun bef var -> bef^" "^(print_expr (func var))) "" vars)^")"
    | F_node(Tr_start(vars), func) -> "(start"^(List.fold_left (fun bef var -> bef^" "^(print_expr (func var))) "" vars)^")"
    | F_node(Tr_end(vars), func) -> "(end"^(List.fold_left (fun bef var -> bef^" "^(print_expr (func var))) "" vars)^")"
    | F_implies(f1, f2) -> "( => "^(print_formula_simple f1)^" "^(print_formula_simple f2)^")"
    | F_and(l) -> if List.length l == 0 then "true" else "( and"^(List.fold_left (fun bef f -> bef^" "^(print_formula_simple f)) "" l)^")"
    | F_bformula(e) -> (print_expr e)
    | F_comment(s) -> ";"^s

       
let rec print_formula_with_assert_forall formula=
  match formula with
  | F_comment(s) -> ";"^s
  | _ -> "(assert (forall ("^ 
     (List.fold_left (fun bef (v_name, t) -> Printf.sprintf "%s(%s %s) " bef v_name (print_type t)) "" (get_used_variables formula)) 
     ^ ") " ^ (print_formula_simple formula) ^ "))"
     
(*prints fun declaration in smt2 format*)
let print_fun_decl (f_name,typesin,typeout)  = 
  Printf.sprintf "(declare-fun %s (%s) %s)" f_name (List.fold_left (fun bef t -> bef ^ (print_type t)^" ") "" typesin) (print_type typeout)

let print_smt_comment mycomment =
  Printf.sprintf ";;%s" mycomment

let print_smt_header mychain = 
  Printf.sprintf "%s" mychain
                 
let print_smt_footer mychain = 
  Printf.sprintf "%s" mychain

      
