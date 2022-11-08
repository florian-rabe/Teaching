{- does not type-check in Haskell, but should be easy to fix -}

{- one inductive type for every non-terminal
   one constructor for every production
   one constructor argument for every non-terminal on the rhs
-}
data N = Zero | One | Sum N N | Product N N
data F = Equals N N | LessEq N N

{- for every non-terminal, one printing function
   for every constructor, one case
   for every constructor arguemtn, one recursive call
-}
let print_N Zero = "zero"
            One = "one"
            Sum x y = (print_N x) + "+" + (print_N y)
            Product x y = (print_N x) + "*" + (print_N y)

    print_F Equals x y = (print_N x) + "=" + (print_N y)
            LessEq x y = (print_N x) + "<=" + (print_N y)

{- for every constructor, for every argument, one partial selector function -}
let sum_left Sum x _ = x
             _ = {- error -}
let sum_right Sum _ x = x
             _ = {- error -}
let product_left Product x _ = x
             _ = {- error -}
let product_right Product _ x = x
             _ = {- error -}
let equals_left Equals x _ = x
             _ = {- error -}
let equals_right Equals _ x = x
             _ = {- error -}
let lesseq_left LessEq x _ = x
             _ = {- error -}
let lesseq_right LessEq _ x = x
             _ = {- error -}



main = 
  let x = Sum One One in
  let y = Equals x x in
  putStrLn (print_F y)