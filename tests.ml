(*
                              CS51 Lab 9
                         Substitution Semantics
                                Tests

 *)

open Lab9 ;;

let unit_test (condition : bool) (msg : string) =
  if condition then print_endline (msg ^ " passed")
  else print_endline (msg ^ " failed");;

let test_free_vars  () =
unit_test ((free_vars (Let ("x", Binop (Plus, Var "x", Var "y"),
                            Binop (Divide, Var "z", Var "x"))))
= [Var "x"; Var "y"; Var "z"]) "free_vars mixed";;

let test_all () =
  test_free_vars () ;;

let _ = test_all () ;;
