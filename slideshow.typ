#set text(lang: "fr")

#import "@preview/touying:0.7.3": *
#import "university.typ": *
#import "@preview/theorion:0.6.0": *
#import cosmos.rainbow: *
#import "@preview/pinit:0.2.2": *
#import "@preview/fletcher:0.5.8"
#import "@preview/tdtr:0.5.5": *
#import "@preview/muchpdf:0.1.2": muchpdf

#show: show-theorion

#show: university-theme.with(
  aspect-ratio: "4-3",
  align: horizon,
  config-info(
    title: [Optimisation de la réduction d'expressions du lambda calcul],
    author: [Hurot Eliott],
    date: [2025-2026],
    logo: [$lambda$]
  ),
)

#set align(center)
#show math.equation: set text(20pt)

#let red(x) = text(fill: color.red, x)

#title-slide()

== Sommaire <touying:hidden>

#set text(30pt)
#components.adaptive-columns(outline(title: none, depth: 1, indent: 2em))
#set text(20pt)

= Définitions

== Syntaxe

$ Lambda 
    &:= x &text("Variable", fill: #color.hsl(0deg, 0, 45%)) \
    &bar M N &text("Application", fill: #color.hsl(0deg, 0, 45%))\
    &bar lambda x . M &text("Fonction", fill: #color.hsl(0deg, 0, 45%)) $
\
$ M, N in Lambda text("et") x in V $
\
Redex : $ (lambda x . u) v $

== Sucre

#set align(left)
$forall n in NN, ceil n ceil.r := lambda f,x . f^n x #h(0.7cm), #h(0.7cm) f^0 t := t #h(1cm) f^(n+1) t := f(f^n t)$ \
#v(1cm)
#columns(2, gutter: 3cm)[
  $ceil S ceil.r := lambda n, f, x . f(n f x)$ \
  $ceil V ceil.r := lambda x, y . x$ \
  $ceil <u, v> ceil.r := lambda z. z u v$

  #colbreak()
  $ceil + ceil.r := lambda m, n, f, x . m f (n f x)$ \
  $ceil F ceil.r := lambda x, y . y$
]
#set align(center)
#v(1cm)

$ ceil + ceil.r ceil 1 ceil.r ceil 2 ceil.r = (lambda m . lambda n . lambda f . lambda x . m f (n f x)) (lambda f . lambda x . f x) (lambda f . lambda x . f (f x)) $

== Réductions

#grid(
  align: top,
  columns: (auto, auto),
  column-gutter: 3cm,
  rows: 1,
[
=== $alpha$-réduction
\
Renommage \
$ lambda x . x + 1 #h(0.5cm) attach(=, t:?) #h(0.5cm) lambda y . y + 1 $

$ lambda x . u #h(1cm) alpha #h(1cm) lambda y . (u[x := y])  #h(1cm) arrow.double #h(1cm) attach(=, br:alpha) #h(1cm) arrow.double #h(1cm) Lambda slash script(attach(=, br:alpha)) $

=== $beta$-réduction
\
Exécution \
$ (lambda x . u) v #h(1cm) beta #h(1cm) u[x := v] #h(1cm) arrow.double #h(1cm) arrow $
],

[
#set text(14pt)
#show math.equation: set text(14pt)
Passe au contexte :
$ cases(delim: "|", reverse: #false, u_1 =_alpha u_2, v_1 =_alpha v_2) &arrow.double u_1 v_1 =_alpha u_2 v_2 \
  u =_alpha v &arrow.double lambda x . u =_alpha lambda x . v $
]
)

= Terminaison et confluence

_Forme normale_ : on ne peut plus simplifier \
#v(1cm)
#underline("Exemple") : \
$ ((lambda x, y . x y) (lambda x . x)) z arrow (lambda y . (lambda x . x) y) z arrow (lambda x . x) z arrow z $
#v(1cm)
#text(size: 25pt, "Existence et Unicité ?") \

== Terminaison

#theorem[Terminaison de la $beta$-réduction][
  La $beta$ réduction ne termine pas toujours
]
#set align(left)
#proof[ \
  $Omega := (lambda x . x x)(lambda x .x x)$ \
  $ Omega &arrow (x x)[x := (lambda x . x x)]
          &arrow (lambda x . x x)(lambda x . x x) = Omega $
]
_Fortement normalisant_ : LES réductions se terminent \
$ ((lambda x, y . x y) (lambda x . x)) z arrow (lambda y . (lambda x . x) y) z arrow (lambda x . x) z arrow z $
_Faiblement normalisant_ : UNE Réduction se termine
$ &(lambda x . y) Omega arrow y \
  &(lambda x . y) Omega arrow (lambda x . y) Omega arrow ... $
#set align(center)

== Confluence

#theorem[Confluence du lambda calcul][
  Si $u arrow^* v_1$ et $u arrow^* v_2$, alors il existe $w$ tel que $v_1 arrow^* w$ et $v_2 arrow^* w$
]

#corollary[Unicité des formes normales][
  Si $u_1$ et $u_2$ sont deux formes normales de $u$, alors $u_1 = u_2$
]

== Terminaison du programme

#theorem[Standardisation][
  Si $u$ est faiblement normalisant, alors la stratégie externe gauche calcule la forme normale de $u$ par une réduction finie.
]
= Stratégies de réductions

== Stratégies internes

$ &u v attach(arrow, tr: *) u' v attach(arrow, tr: *) u' v' \
&u v attach(arrow, tr: *) (lambda x . t) v attach(arrow, tr: *) (lambda x . t) v' arrow t[x := v'] $

#v(1cm)

#columns(2, gutter: 2cm)[
  ```C
  int incr(int x){
    return x + 1;
  }
  int main(){
    int x = 4;
    int y = incr(x);
  }
  ```

  #colbreak()

  ```Ocaml
  let a = ref 0
  let f x y = ()
  let () = f (a:=1) (a:=2); print_int !a
  ```
]

#v(1cm)

$ & ceil "fact" ceil.r ( ceil + ceil.r ceil 1 ceil.r ceil 2 ceil.r ) \
  arrow^* &ceil "if" ceil.r ( ceil "zero ?" ceil.r ceil 3 ceil.r) ceil 1 ceil.r (ceil times ceil.r ceil 3 ceil.r (ceil "fact" ceil.r (ceil P ceil.r ceil 3 ceil.r))) $

== Stratégies externes

$ (lambda x . u) v arrow u[x := v] $
$ & ceil "fact" ceil.r ( red(ceil + ceil.r ceil 1 ceil.r ceil 2 ceil.r) ) \
  arrow^* &(lambda n . ceil "if" ceil.r (ceil "zero ?" ceil.r n) ceil 1 ceil.r (ceil times ceil.r n (ceil "fact" ceil.r (ceil P ceil.r n))))( red(ceil + ceil.r ceil 1 ceil.r ceil 2 ceil.r) ) \
  arrow & ceil "if" ceil.r (ceil "zero ?" ceil.r ( red(ceil + ceil.r ceil 1 ceil.r ceil 2 ceil.r) ))ceil 1 ceil.r (ceil times ceil.r( red(ceil + ceil.r ceil 1 ceil.r ceil 2 ceil.r) )(ceil "fact" ceil.r ( ceil P ceil.r ( red(ceil + ceil.r ceil 1 ceil.r ceil 2 ceil.r) )))) $

= Expressivité

$ ceil "fact" ceil.r =_beta lambda n . ceil "if" ceil.r (ceil "zero ?" ceil.r n) ceil 1 ceil.r (ceil times ceil.r n (ceil "fact" ceil.r (ceil P ceil.r n))) $
$ ceil "fact" ceil.r = F ceil "fact" ceil.r $

#theorem[Existence de point fixe][
  Tout $lambda$-terme $F$ a un point fixe $x$, i.e. $x =_beta F x$
]

#theorem[Equivalence fonction récursive / lambda terme][
  Pour toute fonction récursive $f$, il existe un $lambda$-terme $ceil f ceil.r$ qui code correctement $f$\
  $forall m = (m_1, ..., m_k) in NN^k slash f(m) "est défini",\ ceil f ceil.r(ceil m_1 ceil.r, ..., ceil m_k ceil.r) =_beta ceil f(m) ceil.r$
]

= Programme et Optimisations

== Algorithme

#grid(rows: auto, columns: (auto, auto),
[
#set align(left)
1. Analyse lexicale : \
String \
#sym.arrow.r.curve Tokens \ \
2. Analyse syntaxique : \
Tokens \
#sym.arrow.r.curve Arbre d'analyse \ \
3. Construction du graphe : \
Arbre d'analyse \
#sym.arrow.r.curve Graphe des dérivations possibles \ \
4. Recherche du plus cours chemin : \
Graphe des dérivations possibles \
#sym.arrow.r.curve Plus court chemin du terme initial à sa forme normale
#set align(center)
],
[
\
#sym.quote $( L x . x ) y$ #sym.quote \
#sym.arrow.b.double \
$["LParen"; "Var " x; "Dot"; "Var " x; "RParen"; "Var " y]$ \
#sym.arrow.b.double \
#tidy-tree-graph(
  draw-edge: (
    tidy-tree-draws.horizontal-vertical-draw-edge,
    (stroke: 0.8pt)
  ),
  draw-node: ((label,)) => (stroke: none, label: label),
  spacing: (30pt, 30pt)
)[
  - $"App"$
    - $lambda . x$
      - $x$
    - $y$
] \
#sym.arrow.b.double \
  #muchpdf(
    width: 75%,
    height: 7%,
    read("slideshowPictures/Iy_reduction_graph.pdf", encoding: none),
  )
]
)

== Indice de De Bruijn

$ lambda x . lambda y . x y #h(0.5cm) = #h(0.5cm) lambda#pin("lambda1") . lambda#pin("lambda2") . #pin("index1")1 #pin("index2")0 $

#pinit-fletcher-edge(fletcher, "lambda1", end:"index1", bend:45deg, "<-", start-dy: -0.5cm, stroke: color.red)
#pinit-fletcher-edge(fletcher, "lambda2", end:"index2", bend:45deg, "<-", start-dy: -0.5cm, stroke: color.blue)

Liste d'entier ou renomage des variables #sym.arrow.double Illisible \
Solution : Comparaison structurelle \

#columns(2, gutter: 1cm)[
#tidy-tree-graph(
  draw-edge: (
    tidy-tree-draws.horizontal-vertical-draw-edge,
    (stroke: 0.8pt)
  ),
  draw-node: ((label,)) => (stroke: none, label: label),
  spacing: (30pt, 30pt)
)[
  - $lambda . m$
    - $lambda . n$
      - $lambda . f$
        - $"App"$
          - $m$
          - $"App"$
            - $n$
            - $f$
]
#colbreak()
#tidy-tree-graph(
  draw-edge: (
    tidy-tree-draws.horizontal-vertical-draw-edge,
    (stroke: 0.8pt)
  ),
  draw-node: ((label,)) => (stroke: none, label: label),
  spacing: (30pt, 30pt)
)[
  - $lambda . x$
    - $lambda . y$
      - $lambda . z$
        - $"App"$
          - $x$
          - $"App"$
            - $y$
            - $z$
]
]

== Algorithme de recherche de plus cours chemin

#grid(rows: auto, columns: (3fr, 2fr), gutter: 3cm,
[
  $ceil P ceil.r ceil 10 ceil.r #h(1cm) : #h(1cm) &"Ordre :" 50551 \ &"Taille :" 310 915$
  #v(1cm)
  Quelques complexité :

  - #align(left)[Test d'égalité structurelle : $O(n dot.c v)$]
  - #align(left)[Substitution de $x$ par $v$ dans $u$ : $O(n + m^2)$]
  - #align(left)[Etape de réduction : $O(n^3)$]

  n : nombre de noeuds dans l'arbre \
  v : profondeur maximale des variables liées

  #v(1cm)
  Graphe infini...
  #v(1cm)

  #strike("Dijkstra") #sym.arrow.double $A^*$
],
[
  #muchpdf(
    read("slideshowPictures/predecessor5.pdf", encoding: none),
  )
]
)

== File de priorité

#grid(rows: auto, columns: (auto, auto, auto, auto), inset: 8pt, stroke: 0.5pt,
[], [$ceil P ceil.r ceil 5 ceil.r$], [$ceil P ceil.r ceil 10 ceil.r$], [$ceil "fact" ceil.r ceil 1 ceil.r$],
[Binaire], [0.012s], [3m44s], [],
[Binomial], [0.012s], [3m25s], [],
[2-3], [], [], [],
[Fibonacci], [], [], [],
[Pairing], [], [], []
)

Parallélisation

== Heuristique

Admissibilité : \ \
#grid(rows: auto, columns: (auto, auto, auto, auto), inset: 8pt, stroke: 0.5pt,
[], [$ceil P ceil.r ceil 5 ceil.r$], [$ceil P ceil.r ceil 10 ceil.r$], [$ceil "fact" ceil.r ceil 1 ceil.r$],
[Taille du terme], [15], [25], [],
[Nombre de redex], [15], [], [],
[Nombre de spine redex], [], [], [],)


