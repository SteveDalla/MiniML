open Expr ;;
open CS51Utils ;;
open Absbook ;;

let unit_test (test : bool) (msg : string) : unit =
  if test then
    Printf.printf "%s passed\n" msg
  else 
    Printf.printf "%s failed\n" msg ;;


let free_vars_test () = 
  unit_test (free_vars (Num 1) = vars_of_list [])
    "free_vars1";
  unit_test (free_vars (Var "x") = vars_of_list ["x"])
    "free_vars2" ;
  unit_test (free_vars (Unop (Negate, Num 1)) = vars_of_list [])
    "free_vars3" ;
  unit_test (free_vars (Unop (FloatNegate, Float 1.)) = vars_of_list [])
    "free_vars4" ;
  unit_test (free_vars (Unop (Sine, Float 1.)) = vars_of_list [])
    "free_vars5" ; 
  unit_test (free_vars (Binop (Plus, Num 1, Num 2)) = vars_of_list [])
    "free_vars6" ;
  unit_test (free_vars (Binop (Plus, Var "v", Num 2)) = vars_of_list ["v"])
    "free_vars7" ;
  unit_test (free_vars (Let ("v", Num 1, Var "x")) = vars_of_list ["x"])
    "free_vars8" ;
  unit_test (free_vars (Let ("v", Fun ("x", Binop (Minus, Var "y", Var "z")),
                        App (Var "v", Num 1))) 
            = vars_of_list ["y"; "z"])
    "free_vars9" ;
  unit_test (free_vars (Letrec ("v", Binop (Plus, Var "x", Var "y"), 
                                  Binop (Minus, Var "v", Var "z")))
            = vars_of_list ["x"; "y"; "z"])
    "free_vars10" ;
  unit_test (free_vars (Conditional (Var "x", Num 1, Num 2)) 
            = vars_of_list ["x"])
    "free_vars11" ;
  unit_test (free_vars (Conditional (Var "x", Var "y", Float 1.)) 
            = vars_of_list ["x"; "y"])
    "free_vars12" ;
  unit_test (free_vars (Conditional (Bool true, String "hello", Float 1.)) 
            = vars_of_list [])
    "free_vars13" ;
  unit_test (free_vars (Fun ("v", Binop (Plus, Var "x", Var "y"))) 
            = vars_of_list ["x"; "y"])
    "free_vars14" ;
  unit_test (free_vars (Fun ("v", Conditional(Var "x", Var "y", Var "z")))
            = vars_of_list ["x"; "y"; "z"])
    "free_vars15" ;
  unit_test (free_vars (App (Var "x", Var "y")) = vars_of_list ["x"; "y"])
    "free_vars16" ;
  unit_test (free_vars (App (Fun ("v", Binop (Plus, Var "v", Var "y")), 
                             String "world")) 
            = vars_of_list ["y"])
    "free_vars17" ;
  unit_test (free_vars (App (Fun ("v", Binop (FloatTimes, Var "x", Var "y")), 
                                       Unop (FloatNegate, Var "z")))
            = vars_of_list ["x"; "y"; "z"])
    "free_vars 18" 
;;

let new_varname_test () = 
  unit_test (new_varname () = "var1") 
    "new_varname1" ;
  unit_test (new_varname () = "var2")
    "new_varname2" ;
  unit_test (new_varname () = "var3")
    "new_varname3" ;
  unit_test (new_varname () = "var4")
    "new_varname4"
;;

let subst_test () = 
  unit_test (subst ("x") (Num 1) (Num 2) = Num 2)
    "subst1" ;
  unit_test (subst ("x") (Float 1.) (Var "x") = Float 1.)
    "subst2" ;
  unit_test (subst ("x") (Num 1) (Var "y") = Var "y")
    "subst3" ;
  unit_test (subst ("x") (Num 1) (Unop (Negate, Var "x")) 
            = Unop (Negate, Num 1))
    "subst4" ;
  unit_test (subst ("x") (Num 1) (Binop (Plus, Var "x", Var "y")) 
            = Binop (Plus, Num 1, Var "y"))
    "subst5" ;
  unit_test (subst ("x") (Num 1) (Binop (Plus, Binop (Times, Var "x", Var "y"),
                                               Var "x")) 
            = Binop (Plus, Binop (Times, Num 1, Var "y"), Num 1))
    "subst6" ;
  unit_test (subst ("x") (Num 1) (Let ("x", Num 2, Var "x")) 
            = Let ("x", Num 2, Var "x"))
    "subst7" ;
  unit_test (subst ("x") (Num 1) (Let ("y", Binop (Plus, Var "x", Var "y"), 
                                            Binop (Minus, Var "y", Var "x"))) 
            = Let ("y", Binop (Plus, Num 1, Var "y"), 
                        Binop (Minus, Var "y", Num 1)))
    "subst8" ;
  unit_test (subst ("x") (Num 1) (Conditional (Var "x", Num 1, Num 2))
            = Conditional (Num 1, Num 1, Num 2))
    "subst9" ;
  unit_test (subst ("x") (Num 1) (Conditional (Var "y", Num 1, Num 2))
            = Conditional (Var "y", Num 1, Num 2))
    "subst10" ;
  unit_test (subst ("x") (Num 1) 
            (subst ("y") (Num 2) (Binop (Plus, Var "x", Var "y")))
            = Binop (Plus, Num 1, Num 2))
    "subst11" ;
  unit_test (subst ("x") (Num 1) 
            (subst ("y") (Num 2) (Fun ("z", Binop (Plus, Var "x", Var "y"))))
            = Fun ("z", Binop (Plus, Num 1, Num 2)))
    "subst12"
;;

let exp_to_concrete_string_test () = 
  unit_test (exp_to_concrete_string (Var "x") = "x")
    "exp_to_concrete_string1" ;
  unit_test (exp_to_concrete_string (Num 1) = "1")
    "exp_to_concrete_string2" ;
  unit_test (exp_to_concrete_string (Float 1.) = "1.")
    "exp_to_concrete_string3" ;
  unit_test (exp_to_concrete_string (Bool true) = "true")
    "exp_to_concrete_string5" ;
  unit_test (exp_to_concrete_string (String "hello world") = "\"hello world\"")
    "exp_to_concrete_string6" ;
  unit_test (exp_to_concrete_string (Unop (Negate, Num 1)) = "~-1")
    "exp_to_concrete_string7" ;
  unit_test (exp_to_concrete_string (Unop (FloatNegate, Float 1.)) = "~-.1.")
    "exp_to_concrete_string8" ;
  unit_test (exp_to_concrete_string (Binop (Plus, Var "x", Var "y")) = "x + y")
    "exp_to_concrete_string9" ;
  unit_test (exp_to_concrete_string (Binop (FloatDivide, Var "x", Var "y"))
            = "x /. y")
    "exp_to_concrete_string10" ;
  unit_test (exp_to_concrete_string (Conditional (Var "x", Num 1, Num 2)) 
            = "if x then 1 else 2")
    "exp_to_concrete_string11" ;
  unit_test (exp_to_concrete_string (Let ("x", Num 1, 
                                          Fun ("x", Unop (Negate, Var "x"))))
            = "let x = 1 in fun x -> ~-x")
    "exp_to_concrete_string12" ;
  unit_test (exp_to_concrete_string (App (Var "x", Var "y")) = "x (y)")
    "exp_to_concrete_string13" ;
;;

let exp_to_abstract_string_test () = 
  unit_test (exp_to_abstract_string (Var "x") = "Var(x)")
    "exp_to_abstract_string1" ;
  unit_test (exp_to_abstract_string (Num 1) = "Num(1)")
    "exp_to_abstract_string2" ;
  unit_test (exp_to_abstract_string (Float 1.) = "Float(1.)")
    "exp_to_abstract_string3" ;
  unit_test (exp_to_abstract_string (Bool true) = "Bool(true)")
    "exp_to_abstract_string5" ;
  unit_test (exp_to_abstract_string (String "hello world") = "String(\"hello world\")")
    "exp_to_abstract_string6" ;
  unit_test (exp_to_abstract_string (Unop (Negate, Num 1)) = "Unop(Negate, Num(1))")
    "exp_to_abstract_string7" ;
  unit_test (exp_to_abstract_string (Unop (FloatNegate, Float 1.)) 
            = "Unop(FloatNegate, Float(1.))")
    "exp_to_abstract_string8" ;
  unit_test (exp_to_abstract_string (Binop (Plus, Var "x", Var "y")) 
            = "Binop(Plus, Var(x), Var(y))")
    "exp_to_abstract_string9" ;
  unit_test (exp_to_abstract_string (Conditional (Var "x", Num 1, Num 2)) 
            = "Conditional(Var(x), Num(1), Num(2))")
    "exp_to_abstract_string10" ;
  unit_test (exp_to_abstract_string (Let ("x", Num 1, 
                                          Fun ("x", Unop (Negate, Var "x"))))
            = "Let(x, Num(1), Fun(x, Unop(Negate, Var(x))))")
    "exp_to_abstract_string12" ;
  unit_test (exp_to_abstract_string (App (Var "x", Var "y")) = "App(Var(x), Var(y))")
    "exp_to_abstract_string13" 
;;


let test_all () = 
  free_vars_test () ;
  new_varname_test () ;
  subst_test () ;
  exp_to_concrete_string_test () ;
  exp_to_abstract_string_test () ;;

let _ = test_all () ;;
