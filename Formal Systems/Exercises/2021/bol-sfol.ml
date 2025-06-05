(* BOL *)
type ID = string

type I = ID

type P = ID

datatype V = strV of string | bV of bool | iV of int | fV of real

datatype R = bl_atomic_R of ID
          | bl_r_union of R*R
          | bl_r_intersect of R*R
          | bl_r_composition of R*R
          | bl_r_trans_closure of R
          | bl_r_inv of R
          | bl_r_identity of C

and C = bl_atomic_C of ID
          | bl_union of C*C
          | bl_intersect of C*C
          | bl_forall of R*C
          | bl_exists of R*C
          | bl_dom_r of R
          | bl_rng of R
          | bl_dom_p of P

and F = bl_equals of C*C
          | bl_sub of C*C
          | bl_f_is_a of I*C
          | bl_f_R of ID*I*I
          | bl_f_P of ID*I*V

and D = bl_individual of ID
          | bl_concept of ID
          | bl_relation of ID
          | bl_property of ID
          | bl_is_a of I*C
          | bl_R of ID*I*I
          | bl_P of ID*I*V
          | bl_form of F

and O = onto of D list;

(* SFOL *)

type Y = ID

type VarDecl = ID * Y
(* list of variable declarations, innermost one at the end *)
type Context = VarDecl list

datatype theory = thy of (declaration list)

and declaration = typ of ID
                  | func of ID*(Y list)*Y
                  | pred of ID*(Y list)
                  | axiom of formula

and term = t_app of ID*(term list)
            | var of ID
            | lit of V

and formula = p_app of ID*(term list)
              | equals of Y*term*term
              | truth
              | falsity
              | conj of formula*formula
              | disj of formula*formula
              | impl of formula*formula
              | neg of formula
              | forall of ID*Y*formula
              | exists of ID*Y*formula

(* returns a fresh variable, i.e., a variable not declared in ctx yet *)
fun freshVar(ctx: Context): ID = "???" (* TODO *)

(* Semantics *)

(* BOL to SFOL *)

fun semI(ctx, i:I):term = var i

and semR(ctx, r:R):(term*term -> formula) =
  case r of
    bl_atomic_R(i) => (fn (x, y) =>
      p_app(i, [x, y]))
  | bl_r_union(r1, r2) => (fn (x, y) =>
      disj(semR(ctx, r1)(x, y), semR(ctx, r2)(x, y)))
  | bl_r_intersect(r1, r2) => (fn (x, y) =>
      conj(semR(ctx, r1)(x, y), semR(ctx, r2)(x, y)))
  | bl_r_composition(r1, r2) => let
     val vr = freshVar(ctx)
     val ctxI = ctx @ [(vr, "iota")]
     in (fn (x, y) =>
       exists(vr, "iota", conj(semR(ctxI, r1)(x, var vr), semR(ctxI, r2)(var vr, y))))
    end
  | bl_r_inv(r) => (fn (x, y) =>
     semR(ctx, r)(y, x))
  | bl_r_identity(c) => (fn (x, y) =>
     conj(equals("iota", x, y), semC(ctx, c)(x)))
  | bl_r_trans_closure(r) => (fn (x, y) =>
     falsity) (* TODO: error *)

and semP(ctx, p:P):(term*term -> formula) =
  case p of
    i => (fn (x, y) => p_app(i, [x, y]))

and semC(ctx, c:C):(term -> formula) =
  case c of
    bl_atomic_C i => (fn x => p_app(i, [x]))
  | bl_union(c1, c2) => (fn x =>
     disj(semC(ctx, c1)(x), semC(ctx, c2)(x)))
  | bl_intersect(c1, c2) => (fn x =>
     conj(semC(ctx, c1)(x), semC(ctx, c2)(x)))
  | bl_forall(r, c) => let
      val vr = freshVar(ctx)
      val ctxI = ctx @ [(vr, "iota")]
      in (fn x => forall(vr, "iota", impl(semR(ctxI, r)(x, var vr), semC(ctxI, c)(var vr))))
     end
  | bl_exists(r, c) => let
     val vr = freshVar(ctx)
     val ctxI = ctx @ [(vr, "iota")]
     in (fn x => exists(vr, "iota", conj(semR(ctxI, r)(x, var vr), semC(ctxI, c)(var vr))))
    end
  | bl_dom_r(r) => let
     val vr = freshVar(ctx)
     val ctxI = ctx @ [(vr, "iota")]
     in (fn x => exists(vr, "iota", semR(ctxI, r)(x, var vr)))
    end
  | bl_rng(r) => let
    val vr = freshVar(ctx)
    val ctxI = ctx @ [(vr, "iota")]
    in (fn x => exists(vr, "iota", semR(ctxI, r)(var vr, x)))
    end
  | bl_dom_p(p) => let
    val vr = freshVar(ctx)
    val ctxI = ctx @ [(vr, "iota")]
    in (fn x => exists(vr, "T", semP(ctxI, p)(x, var vr)))
    end

and semF(ctx, f:F):formula =
  case f of
    bl_equals(c1, c2) => let
     val x = freshVar(ctx)
     val ctxI = ctx @ [(x, "iota")]
     val c1T = semC(ctxI, c1)(var x)
     val c2T = semC(ctxI, c2)(var x)
     in forall(x, "iota", conj(impl(c1T, c2T), impl(c2T, c1T)))
    end
  | bl_sub(c1, c2) => let
    val x = freshVar(ctx)
    val ctxI = ctx @ [(x, "iota")]
    in forall(x, "iota", impl(semC(ctxI, c1)(var x), semC(ctxI, c2)(var x)))
   end
  | bl_f_is_a(i, c) => semC(ctx, c)(semI(ctx, i))
  | bl_f_R(id, i1, i2) => semR(ctx, bl_atomic_R(id))(semI(ctx, i1), semI(ctx, i2))
  | bl_f_P(id, i, v) => semP(ctx, id)(semI(ctx, i), lit(v))


and semD(d:D):declaration = 
  case d of
    bl_individual i => func(i, nil, "iota")
  | bl_concept i => pred(i, ["iota"])
  | bl_relation i => pred(i, ["iota", "iota"])
  | bl_property i => pred(i, ["iota"])
  | bl_is_a(i, c) => axiom(semC(nil, c)(semI(nil, i)))
  | bl_R(id, i1, i2) => axiom(semR(nil, bl_atomic_R(id))(semI(nil, i1), semI(nil, i2)))
  | bl_P(id, i, v) => axiom(semP(nil,id)(semI(nil, i), lit(v)))
  | bl_form(f) => axiom(semF(nil, f)) 

and semDlst(l:(D list)):(declaration list) =
  case l of
    nil => nil
  | h :: t => semD(h) :: semDlst(t)

and semO(ont:O):theory = 
  case ont of
    onto(l) => thy((typ "iota") :: semDlst(l));
