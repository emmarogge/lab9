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
  unit_test
    ((free_vars (Let ("x", Binop (Plus, Var "x", Var "y"),
                      Binop (Divide, Var "z", Var "x"))))
     = VarSet.(empty
               |> add "x"
               |> add "y"
               |> add "z"))
    "free_vars mixed" ;;

let test_subst () =
  unit_test (
    let example = Let ("x", Binop (Plus, Var "x", Var "y"),
                       Binop (Times, Var "z", Var "x")) in
    subst example "x" (Var "q") = Let ("q", Binop (Plus, Var "x", Var "y"),
                                       Binop (Times, Var "z", Var "q"))) "test_subst var4var";;

let test_all () =
  test_free_vars () ;
  test_subst () ;;

let _ = test_all () ;;
