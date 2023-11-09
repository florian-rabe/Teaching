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
print_N Zero = "zero"
print_N One = "one"
print_N (Sum x y) = (print_N x) ++ "+" ++ (print_N y)
print_N (Product x y) = (print_N x) ++ "*" ++ (print_N y)

print_F (Equals x y) = (print_N x) ++ "=" ++ (print_N y)
print_F (LessEq x y) = (print_N x) ++ "<=" ++ (print_N y)

{- for every constructor, for every argument, one partial selector function -}
{- The functions are partial because cases are missing; 
   this will cause run-time errors if called on objects that are not an instance of the respective constructor.
   In Java, we do not have this issue because every constructor is a class and thus its own type.
-}
sum_left (Sum x _) = x
sum_right (Sum _ x) = x
product_left (Product x _) = x
product_right (Product _ x) = x
equals_left (Equals x _) = x
equals_right (Equals _ x) = x
lesseq_left (LessEq x _) = x
lesseq_right (LessEq _ x) = x

main = 
  let x = Sum One One in
  let y = Equals x x in
  putStrLn (print_F y)