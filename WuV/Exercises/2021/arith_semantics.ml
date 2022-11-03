(* Syntax:

  one inductive data type per non-terminal

  for every non-terminal:
    one constructor per production

  for every production:
    one constructor argument for non-terminal on the rhs of the production

*)

datatype N = zero
           | one
           | plus of N*N
           | times of N*N

     and F = equal of N*N
           | leq of N*N

val example: N =
  times(
    plus(one,one),
    plus(one,plus(one,one))
  )
;

times(example,example);
plus(it,one);

(* Semantics

  one induction function for every non-terminal
  
  for every non-terminal:
    one case per production

    for every production:
      one recursive call per non-terminal on the rhs
*)

fun semN(zero) = ""
  | semN(one) = "|"
  | semN(plus(m,n)) = semN(m) ^ semN(n)
  | semN(times(m,n)) = String.translate(fn x => semN n) (semN m)

;

fun semF(equal(m,n)) = (semN m) = (semN n)
  | semF(leq(m,n)) = String.isPrefix (semN m) (semN n)
;

semN(example);
semF(leq(one,example));

(* alternative function definitions without syntactic sugar *)

fun semNA(x:N): string =
 case x of
    zero => ""
  | one => "|"
  | plus(m,n) => (semNA m) ^ (semNA n)
  | times(m,n) => String.translate(fn x => semNA n) (semNA m)

fun semFA(x: F): bool =
 case x of
    equal(m,n) => (semNA m) = (semNA n)
  | leq(m,n) => String.isPrefix (semNA m) (semNA n)
;

semNA(example);
semFA(leq(one,example));
