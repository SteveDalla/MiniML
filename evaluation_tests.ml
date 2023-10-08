open Expr ;;
open Evaluation ;;
open CS51Utils ;;
open Absbook ;;

let unit_test (test : bool) (msg : string) : unit =
  if test then
    Printf.printf "%s passed\n" msg
  else 
    Printf.printf "%s failed\n" msg ;;

let env = Env.extend (Env.empty ()) "x" (ref (Env.Val (Num 1))) ;;

let env_module_test () =
  unit_test (Env.env_to_string (Env.empty ()) = "") 
    "env_module1" ;
  unit_test (Env.close (Var "x") (Env.empty ())
            = Closure (Var "x", Env.empty ()))
    "env_module2" ;
  unit_test (Env.lookup env "x" = Env.Val (Num 1))
    "env_module3" ;
  unit_test (Env.env_to_string (Env.extend (Env.empty ()) "x" (ref (Env.Val (Float 1.)))) 
            = "[x -> 1.]")
    "env_module4" 
;;

let empty = Env.empty () ;;

let eval_s_test () = 
  unit_test (eval_s (Num 1) empty = Env.Val (Num 1))
    "eval_s1" ;
  unit_test (eval_s (Float 1.) empty = Env.Val (Float 1.))
    "eval_s2" ;
  unit_test (eval_s (Bool true) empty = Env.Val (Bool true))
    "eval_s3" ;
  unit_test (eval_s (String "hello world") empty = Env.Val (String "hello world"))
    "eval_s4" ;
  unit_test (eval_s (Binop (Plus, Num 1, Num 2)) empty = Env.Val (Num 3))
    "eval_s5" ;
  unit_test (eval_s (Let("x", Num(1), Let("f", Fun("y", Binop(Plus, Var("x"), Var("y"))), 
                        Let("x", Num(2), App(Var("f"), Num(3)))))) empty 
            = Env.Val (Num 4))
    "eval_s6" ; 
  unit_test (eval_s (Letrec("f", Fun("n", Conditional(Binop(Equals, Var("n"), Num(0)),
                                  Num(1), Binop(Times, Var("n"), 
                                          App(Var("f"), 
                                          Binop(Minus, Var("n"), Num(1)))))),
                           App(Var("f"), Num(2)))) empty
            = Env.Val (Num 2))
    "eval_s7" ;
;;

let eval_d_test () = 
  unit_test (eval_d (Num 1) empty = Env.Val (Num 1))
    "eval_d1" ;
  unit_test (eval_d (Float 1.) empty = Env.Val (Float 1.))
    "eval_d2" ;
  unit_test (eval_d (Bool true) empty = Env.Val (Bool true))
    "eval_d3" ;
  unit_test (eval_d (String "hello world") empty = Env.Val (String "hello world"))
    "eval_d4" ;
  unit_test (eval_d (Binop (Plus, Num 1, Num 2)) empty = Env.Val (Num 3))
    "eval_d5" ;
  unit_test (eval_d (Let("x", Num(1), Let("f", Fun("y", Binop(Plus, Var("x"), Var("y"))), 
                        Let("x", Num(2), App(Var("f"), Num(3)))))) empty 
            = Env.Val (Num 5))
    "eval_d6" ; 
  unit_test (eval_d (Letrec("f", Fun("n", Conditional(Binop(Equals, Var("n"), Num(0)),
                                  Num(1), Binop(Times, Var("n"), 
                                          App(Var("f"), 
                                          Binop(Minus, Var("n"), Num(1)))))),
                           App(Var("f"), Num(2)))) empty
            = Env.Val (Num 2))
    "eval_d7" ;
  unit_test (eval_d (Let("x", Num(2), 
                      Let("f", Fun("y", 
                                   Binop(Times, Var("x"), Var("y"))), 
                                   Let("x", Num(1), App(Var("f"), Num(21)))))) 
                                   empty 
            = Env.Val (Num 21))
    "eval_d8" ;
;;


let eval_l_test () = 
  unit_test (eval_l (Num 1) empty = Env.Val (Num 1))
    "eval_l1" ;
  unit_test (eval_l (Float 1.) empty = Env.Val (Float 1.))
    "eval_l2" ;
  unit_test (eval_l (Bool true) empty = Env.Val (Bool true))
    "eval_l3" ;
  unit_test (eval_l (String "hello world") empty = Env.Val (String "hello world"))
    "eval_l4" ;
  unit_test (eval_l (Binop (Plus, Num 1, Num 2)) empty = Env.Val (Num 3))
    "eval_l5" ;
  unit_test (eval_l (Let("x", Num(1), Let("f", Fun("y", Binop(Plus, Var("x"), Var("y"))), 
                      Let("x", Num(2), App(Var("f"), Num(3)))))) empty 
            = Env.Val (Num 4))
    "eval_l6" ; 
  unit_test (eval_l (Let("x", Num(2), 
                         Let("f", Fun("y", 
                                      Binop(Times, Var("x"), Var("y"))), 
                                      Let("x", Num(1), App(Var("f"), Num(21)))))) 
                                      empty 
            = Env.Val (Num 42))
    "eval_l7" ;
;;

let test_all () = 
  env_module_test () ;
  eval_s_test () ;
  eval_d_test () ;
  eval_l_test () ;;

let _ = test_all () ;;
