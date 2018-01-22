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

(* This file is a use case demo *)

open SmtOCamlDump
open Int64
  
let debug = false
let outputsmt_name = "test.smt2"

let test1 =
  F_comment("this a comment")
  
let test2 = F_and(F_bformula(Fe_bool(true))::[F_bformula(Fe_bool(false))])

let vx = Fe_variable({ trans_initial_var = "x"; trans_type =  Tr_int})

let va = Fe_variable({ trans_initial_var = "a";
		       trans_type =  Tr_array(Tr_int)})

(* 72==x+42*)
let test2 = F_bformula(Fe_binary(Fe_eq,Fe_int(of_int 72),
				 Fe_binary(Fe_add, vx,Fe_int(of_int 42))))

(* a[7]>42 *)
let testarray = F_bformula(Fe_binary(Fe_gt,Fe_select(va,Fe_int(of_int 7)),
				     Fe_int(of_int 42)))

let onedecl = ("myfavoritefunctionname",[Tr_int;Tr_bool;Tr_array(Tr_int)],Tr_int)

let mycomment = "This is a testfile"
  
let myheader = "(set-option:timeout 300000)\n(set-logic HORN)"
let myfooter = "\n\n(check-sat)"


let test3 = F_node(Tr_command("toto", [{trans_initial_var = "x"; trans_type =  Tr_int}]),fun v -> Fe_variable(v))

(*main function*)
let _ =
  let mytest = test3 in (*change here!*)
  let _ = Printf.printf("Printing the test!\n") in
  let
    strheader = Printf.sprintf "%s\n" (print_smt_header myheader) and
    strfooter = Printf.sprintf "%s\n" (print_smt_footer myfooter) and
    strcomment  = Printf.sprintf "%s\n" (print_smt_comment mycomment) and
    strdecl  = Printf.sprintf "%s\n" (print_fun_decl onedecl) and
    str = Printf.sprintf "%s\n" (print_formula_with_assert_forall mytest)  in
  if debug then Printf.printf "%s%s" strdecl str; 
  let outfile = open_out outputsmt_name in
  Printf.fprintf outfile "%s%s%s%s%s" strheader strcomment strdecl str strfooter;
  close_out outfile;
