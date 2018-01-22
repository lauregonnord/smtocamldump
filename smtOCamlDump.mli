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

    
(* Binary operators. *)
type fe_binary_op =
  | Fe_add | Fe_sub | Fe_mul | Fe_div | Fe_mod
  | Fe_or | Fe_and | Fe_xor
  | Fe_lt | Fe_le | Fe_gt | Fe_ge
  | Fe_eq
      
type f_var = string (*can be anything*)

  
(*abstract variable*)
type abstract_var =
{ 
  trans_initial_var : f_var;
  trans_type : abstract_type;
}

(*Abstract commands*)
type abstract_command =
| Tr_start of abstract_var list
| Tr_end of abstract_var list
(*| Tr_command of s_command_e * (abstract_var list) (*the abstract var list is the list of variables used by the command*)*)


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


(* dump functions *)                 
val print_expr : abstract_expr -> string
val print_formula_simple : formula -> string
val print_formula_with_assert_forall : formula -> string
                                        
