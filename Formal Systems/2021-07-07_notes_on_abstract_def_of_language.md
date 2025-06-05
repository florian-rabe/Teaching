# Formal language/formal system/type theory L

Language consists of
* a set of vocabularies Voc
   written |-_V
* for every V,
   - a set of V-contexts G in Cont_V
     written |-_V G 
   - and for any two contexts G,G', a set of substitutions Sub_V(G,G')
     written |-_V g: G->G'
   typically:
     contexts = list of variable declarations
     substitutions = lists of assignments to the variables
* for every V and every V-context G, a set of expression e in Exp_V(G)
* some flexibility regarding type system, e.g.,
   a binary relation e:E on expressions from Exp_V(G)
   written as G |-_V     e : E 
   or             G |-^L_V e : E  (if L must be made explicit)

   Define Exp_V(G,E) = set of all e such that G |-_V e:E

* often also a distinguished prop
   F is called proposition if |- F: prop
* often also distinguished propositions e =_E e' for some/all E

## Implementing Languages in an OOPL

abstract class Language {type Voc: Type  type Ctx: Voc->Type  type Sub: (V:Voc)*Cont(V)*Cont(V)-> Type  type Exp: (V:Voc)*(G:Cont(V)->Type}
abstract class LanguageWithPropositions extends Language {...}
abstract class LanguageWithEquality extends LanguageWithPropositions {...}

class GrammarLanguage(NonTerminals, Productions, VocSymbol, ContSymbol, SubSymbol, ExpSymbols) extends Language {
  type Voc = anything derived from VocSymbol
  type Cont = anything derived from ContSymbol
  type Sub = anything derived from SubSymbol
  type Exp = anything derived from any of the ExpSymbols
  fun wfv: Voc -> Boolean                                                                          // implements |-_V
  fun wfc: (V:Voc) * Cont(V) -> Boolean                                                     // implements |-_V G
  fun wfs: (V:Voc) * (G:Ctx)*(G':Cont(V)) * Sub(V,G,G') -> Boolean           // implements |-_V  g: G -> G'
  fun wft_check: (V:Voc) * (G:Cont(V)) * Exp(V,G) * Exp(V,G) -> Boolean  // implements G|-_V e:E
  fun wft_infer: (V:Voc) * (G:Cont(V)) * Exp(V,G) -> Exp(V,g) option          // returns E such that G |-_e:E 
}

Note: When finding a grammar (e.g., in a textbook or the lecture notes), parts that are needed down the line may be missing, e.g.,
 * people like to not have contexts, let alone substitutions, if they don't understand how important they are
 * we often have to extend the grammar to allow for more queries, i.e., we need other/more productions for the queries than for vocabularies,
   Example: we may want to add conjunction to BOL
   

# Semantics
   
Additionally for

deductive semantics:
* for every V, G a set Thm_V(G) subset Exp_V(G,prop)
   written G |-_V F

computational semantics:
* for every V, G a function Eval_V(G): Exp_V(G)->Exp_V(G)
   written G |-_V e --> e'

concrete semantics:
* for every V, G, and G|-_V F a set Inst_V(G,F) subset Sub_V(G,empty)


abstract DedSemLang extends LanguageWithPropositions {
  fun InThm: (V:Voc)*(G:Ctx)*Exp(V,G) -> Boolean
}
abstract CompSemLang extends Language {
  fun Eval: (V:Voc)*(G:Ctx)*Exp(V,G) -> Exp(V,G)
}
abstract ConcSemLang extends Language {
  fun Inst: (V:Voc)*(G:Ctx)*Exp(V,G) -> Sub(V,G,empty) set
}


# Language Translations

Translation T from L to M consists of

* a function Voc^T : Voc^L -> Voc^M
   notation: T(V) instead of Voc^T(V)
* for every V in Voc^L
   - a function Cont^T: Cont^L_V -> Cont^M_(T(V))
     notation: T(G) instead of Cont^T(G)
   - a function Sub^T: Sub^L_V(G,G') -> Sub^M_(T(V))(T(G),T(G'))
     notation: T(g) instead of Sub^T(G)
  typically:
     contexts translated compositionally,       e.g., x:E |---> x:Exp^T(E) 
     substitutions translated compositionally, e.g., x/e |---> x/Exp^T(e)
* for every V in Voc^L, G in Cont^L(V)
   a function Exp^T: Exp^L_V(G) -> Exp^M_(T(V))(T(G))

* such that it preserves typing, i.e.,
   if    G |-^L_V e: E   then    T(G) |-^M_(T(V)) T(e) : T(E)

* if L has propositions, we usually need an M-operator True that maps translations of L propoisitions to M-propositions


# Translations in the presence of semantics

Option 1:
  * use translation T:L->M and semantics of M to define semantics of L
  * translate L-judgment/queries to M, answer in M, translate answers back

  Ex: Define |-^L F   by  |-^M  True T(F)

Option 2:
  * compare a semantics for L and the semantics obtained via (1)

  Ex: Soundness+completeness means |-^L F   iff  |-^M  True T(F)
