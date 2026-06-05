#import "@preview/muchpdf:0.1.2": *
#import "@preview/theorion:0.6.0": *
#import cosmos.rainbow: *
#show: show-theorion
#set-inherited-levels(0)
#let definition = definition.with(fill: blue.darken(10%))

// ─ Paramètres globaux ──────────────────────────────────────────────────────
#set document(title: "Optimisation de la réduction d'expressions du lambda-calcul")
#set page(
  paper: "a4",
  margin: (top: 2.5cm, bottom: 2.5cm, left: 2.5cm, right: 2.5cm),
  numbering: "1",
  number-align: center,
)
#set heading(numbering: "1.")
#set text(font: "New Computer Modern", size: 11pt, lang: "fr")
#set par(justify: true)


// ── Raccourcis mathématiques ─────────────────────────────────────────────────
#let eqalpha = $attach(=, br: alpha)$
#let bred = $attach(arrow.r, br: beta)$
#let breds = $attach(arrow.r, br: beta, tr: \*)$
#let bpar = $attach(arrow.r, br: parallel)$
#let sugar(x) = $ceil #x ceil.r$

// ----------------------------------------------------------------------------
#align(center)[
  #v(0.2cm)
  #text(size: 22pt, weight: "bold")[Optimisation de la réduction d'expressions du lambda-calcul]
  #v(0.3em)
  #text(size: 14pt)[HUROT Eliott (_30348_) — MPI, 2025–2026]
  #v(1cm)
]

#set text(size: 12pt)
#v(1cm)
#text(weight: "bold")[Table des matières]
#v(0.3em)
#outline(title: none, depth: 2)
#v(1cm)
#text(weight: "bold")[Introduction]
#v(0.3em)
Le lambda-calcul, introduit par Alonzo Church dans les années 1930, formalise la notion
de calcul par application et substitution de variables. Il constitue le fondement théorique
des langages fonctionnels modernes tels que Haskell ou OCaml @barendregt @goubault,
dans lesquels l'exécution d'un programme correspond précisément à la _béta-réduction_ d'un
lambda-terme. \
Une propriété fondamentale de ce modèle est que la forme normale d'un terme — son résultat
de calcul — est unique lorsqu'elle existe (@thm:confluence[-]). En revanche, les _chemins_ pour l'atteindre
sont multiples, et leur longueur peut varier considérablement. \
L'objectif est donc : 
- implémenter un interpréteur supportant diverses stratégies;
- construire le graphe de réductions de manière paresseuse;
- déterminer la réduction minimale par A\*.
#pagebreak()

// ════════════════════════════════════════════════════════════════════════════
= Fondements théoriques

== Syntaxe et réductions

#definition("Lambda Terme", number: 1)[
  L'ensemble $Lambda$ des *lambda-termes* est défini inductivement par :
  $ Lambda := x quad bar.v quad M N quad bar.v quad lambda x . M $
  où $x$ parcourt un ensemble dénombrable de variables $V$, $M N$ est l'_application_ de $M$
  à $N$, et $lambda x . M$ est l'_abstraction_ de $x$ dans $M$.
  Un sous-terme de la forme $(lambda x . u)\,v$ est appelé un *redex*.
]

L'*alpha-équivalence* $eqalpha$ identifie les termes ne différant que par le renommage
cohérent de variables liées : $lambda x . x eqalpha lambda y . y$. On travaille dans le
quotient $Lambda slash script(eqalpha)$, implémenté par une comparaison structurelle.

#definition("Béta-réduction", number : 2)[
  La *béta-réduction* $bred$ est la plus petite relation sur $Lambda slash script(eqalpha)$ vérifiant :
  $ (lambda x . u)v bred u[x := v] $
  et stable par contexte (application et abstraction). Sa clôture réflexive-transitive est
  notée $breds$. Un terme sans redex est appelé *forme normale*.
]

La *béta-réduction ne termine pas en général* : le terme $Omega := (lambda x . x x)(lambda x . x x)$
satisfait $Omega bred Omega$ et n'a pas de forme normale.

== Terminaison et confluence

On distingue deux niveaux de normalisation :
- *Fortement normalisant (SN)* : toutes les suites de réductions de $u$ sont finies.
- *Faiblement normalisant (WN)* : il existe au moins une suite finie menant à une forme normale.

#theorem("Confluence", number: 1)[
  Si $u breds v_1$ et $u breds v_2$, alors il existe $w$ tel que $v_1 breds w$ et $v_2 breds w$.
] <thm:confluence>

#proof[
  On introduit la *réduction parallèle* $bpar$, qui contracte simultanément un ensemble de
  redex disjoints. Elle satisfait $bred space subset.eq space bpar space subset.eq space breds$.
  L'étape clé est la *propriété du diamant pour $bpar$* : si $u bpar v_1$ et $u bpar v_2$,
  il existe $w$ tel que $v_1 bpar w$ et $v_2 bpar w$. Ce $w$ s'obtient en contractant dans
  $v_1$ (resp. $v_2$) les redex créés par la réduction vers $v_2$ (resp. $v_1$) ; la
  compatibilité des substitutions parallèles assure la cohérence. La confluence de $breds$
  s'obtient alors par induction sur la longueur des séquences @barendregt[ch. 3].
]

#corollary[
  La forme normale d'un terme, lorsqu'elle existe, est *unique à $eqalpha$ près*.
  Ce corollaire justifie la notion de chemin _optimal_ : tous les chemins mènent au même
  but unique.
]

== Stratégies de réduction et standardisation

Une *stratégie* est une fonction sélectionnant le prochain redex à contracter. Les deux
stratégies canoniques sont :

- *Stratégie interne* (appel par valeur) : on évalue l'argument avant substitution.
  Correspond aux langages stricts (C, OCaml par défaut).
- *Stratégie externe gauche* (ordre normal) : on réduit le redex le plus à gauche le plus
  à l'extérieur. C'est la seule stratégie garantie de terminer lorsqu'une forme normale existe.

Ces stratégies ne sont pas équivalentes : $(lambda x . y)\,Omega bred y$ en stratégie externe
(1 pas), mais diverge en stratégie interne.

#theorem("Standardisation", number: 2)[
  Si $u$ est faiblement normalisant, la stratégie externe gauche calcule
  la forme normale de $u$ en un nombre fini de pas.
]

#proof[
  Une *réduction standard* est une séquence où aucun redex n'est contracté à gauche d'un redex
  précédemment contracté. Le théorème de standardisation (Curry, 1958) affirme que toute
  réduction peut être réordonnée en réduction standard @goubault. La stratégie externe
  gauche produit précisément de telles séquences. Si le terme est WN, la réduction standard
  doit atteindre la forme normale : une réduction standard bloquée sur un terme non-normal
  contiendrait un redex contractable à droite, contradiction. La terminaison découle d'un
  ordre bien fondé sur la position du redex contracté à chaque étape.
]

*Conséquence.* Le théorème de standardisation garantit que notre programme termine pour
tout terme WN, même si A\* ne trouve pas le chemin optimal dans le temps imparti.

= Implémentation de l'interpréteur
\
La gestion de l'alpha-équivalence est cruciale : sans elle, des termes identiques seraient
traités comme distincts dans le graphe de réductions, causant des explosions combinatoires.
La comparaison structurelle des termes permet de pallier ce problème efficacement.

== Algorithme

Le programme transforme une chaîne de caractères en chemin optimal par quatre étapes :

#figure(
  table(
    columns: (auto, 1fr, 1fr),
    stroke: (x, y) => if y == 0 { (bottom: 0.8pt) } else { (bottom: 0.4pt) },
    inset: (x: 8pt, y: 6pt),
    align: (center, left, left),
    table.header(
      [*Étape*], [*Transformation*], [*Note de complexité*],
    ),
    [1], [Chaîne $arrow.r$ Tokens], [$O(n)$],
    [2], [Tokens $arrow.r$ AST], [$O(n)$],
    [3], [AST $arrow.r$ Graphe], [$O(n^3)$ par sommet],
    [4], [Graphe $arrow.r$ Plus court chemin], [$O(M log(N))$],
  ),
)

La construction du graphe est *paresseuse* : les successeurs d'un nœud ne sont
calculés que lors de son extraction de la file de priorité. Cette approche est indispensable
car le graphe peut être infini (terme divergent) ou d'une taille rédhibitoire
($sugar(P)sugar(10)$ : 50 000 sommets et 310 000 arêtes).

Les complexités des opérations élémentaires (avec $n$ = nombre de nœuds, $m$ = taille du terme
substitué) sont :

#figure(
  table(
    columns: (1fr, auto),
    stroke: (x, y) => if y == 0 { (bottom: 0.8pt) } else { (bottom: 0.4pt) },
    inset: (x: 8pt, y: 5pt),
    table.header([*Opération*], [*Complexité*]),
    [Égalité structurelle], [$O(n dot v)$],
    [Substitution de $x$ par $v$ dans $u$], [$O(n + m^2)$],
    [Étape de réduction complète], [$O(n^3)$],
  ),
)

= Recherche du chemin optimal par A\*

== Formulation comme problème de plus court chemin

Le *graphe de réductions* $G_u$ d'un terme $u$ est le graphe orienté dont les sommets sont
les termes accessibles depuis $u$ par $breds$, et les arêtes sont les béta-réductions
élémentaires. La forme normale de $u$, si elle existe, est l'unique
puits de ce graphe (par confluence). Le problème est donc : trouver le chemin de poids
minimal de $u$ vers sa forme normale dans $G_u$.

== Algorithme A\*

#theorem("Optimalité de A*", number: 3)[
  L'algorithme A\* retourne un plus court chemin de la source
  vers le but si et seulement si l'heuristique $h$ est *admissible* :
  $forall u, h(u) <= d^*(u)$, où $d^*(u)$ est le nombre minimum de réductions
  pour normaliser $u$.
]

A\* sélectionne à chaque étape le nœud minimisant $f(u) = g(u) + h(u)$, où $g(u)$ est le
nombre de réductions déjà effectuées depuis le terme initial. \

L'exemple $(lambda x. ((lambda x. sugar(I)) Omega) (sugar(+) x x))(sugar(+) sugar(1) sugar(2))$ illustre
la non optimalité des stratégies de réduction basiques :

#figure(
  table(
    columns: (1fr, auto),
    stroke: (x, y) => if y == 0 { (bottom: 0.8pt) } else { (bottom: 0.4pt) },
    inset: (x: 8pt, y: 6pt),
    align: (left, center, center),
    table.header([*Méthode*], [*Taille de la réduction*]),
    [Stratégie interne], [$+oo$],
    [Stratégie externe gauche], [$21$],
    [A\* (heuristique $h_2$)], [$15$],
  ),
)

== Heuristiques admissibles pour le lambda-calcul

Plusieurs heuristiques ont été testées mais 2 seulement se sont révélées admissibles et celle retenue est fondée sur la notion
de spine redex @needed.

*$h(u) = $ Nombre de spine redex de $u$*
#definition("Redex Nécessaire", number: 3)[
  Un redex $R$ dans un terme $M$ est dit *nécessaire* si, dans *toute* réduction de $M$ menant à sa forme normale, un résidu de $R$ est contracté.
]

*Conséquence pour A\* :* Le nombre de ces redex nécessaires constitue une borne inférieure de la distance minimale de $u$ à sa forme normale. Dès lors,
l'heuristique h est admissible.

#theorem("Indécidabilité", number: 4)[
  Déterminer si un redex quelconque est nécessaire est indécidable.
]

On ne peut donc pas déterminer leur nombre directement mais les algorithmes et la notion de spine redex développés par Barendregt 
permet d'obtenir une borne inférieure de ce nombre de redex nécessaire @needed.

== Optimisations notables

L'application de l'algorithme A\* au lambda calcul a permis d'obtenir deux optimisations spécifiques au lambda-calcul qui, bien que dépendantes des termes, peuvent se révéler bénéfiques :
- L'utilisation d'une comparaison secondaire lors d'une égalité dans la file de priorité permet d'orienter A\* en préservant sa correction;
- Lorsqu'un terme est en forme normale de tête $lambda x_1,...,x_n . x u_1 ... u_m$ les réductions des $u_i$ sont indépendantes, dès lors, réduire $u_i$ avant ou après $u_j$ ne change pas la taille de la réduction minimale. Ainsi en forçant la réduction de ces termes de gauche à droite, on retire dans le graphe tous les chemins constituant des permutations des réductions des $u_i$ sans briser la correction de A\*.

#figure(
  table(
    columns: (auto, auto, auto, auto, auto, auto, auto),
    stroke: (x, y) => if y == 0 { (bottom: 0.8pt) } else { (bottom: 0.4pt) },
    inset: (x: 8pt, y: 6pt),
    align: (left, center, center, center, center, center, center),
    table.header(
      table.cell(rowspan: 2, align: horizon)[*Optimisation*],

      table.cell(colspan: 2)[#text(size: 10pt, $chevron.l sugar(+)sugar(2)sugar(2) , sugar(*)sugar(2)sugar(2) chevron.r$)],
      table.cell(colspan: 2)[#text(size: 10pt, $(lambda x . (lambda y . sugar(I)) Omega (sugar(+) x x) )(sugar(+) sugar(1) sugar(2))$)],
      table.cell(colspan: 2)[#text(size: 10pt, $sugar(P)sugar(8)$)],

      [Temps], [$|V|$], [Temps], [$|V|$], [Temps], [$|V|$],
    ),
    [Aucune], [0,138s], [182], [11s], [4 568], [29s], [7 377],
    [Comparaison bis], [0,137s], [181], [10s], [4 521], [29s], [7 375],
    [Ordre de \ réduction], [0,119s], [26], [10s], [4 568], [28s], [7 377],
    [Les deux], [0,116s], [26], [10s], [4521], [29s], [7 375],
  )
)

== Structures de données pour la file de priorité

L'efficacité de A\* repose sur celle de la file de priorité (extraction du minimum et
insertion). \
Les complexités théoriques divergent : le tas de Fibonacci offre $O(1)$ amorti en
insertion et $O(log n)$ en extraction, contre $O(log n)$ pour le tas binaire dans les
deux cas @dheap. L'avantage pratique dépend toutefois de la localité mémoire
et des constantes cachées, d'où la nécessité de tests réels :

#figure(
  table(
    columns: (1fr, auto, auto, auto),
    stroke: (x, y) => if y == 0 { (bottom: 0.8pt) } else { (bottom: 0.4pt) },
    inset: (x: 8pt, y: 6pt),
    align: (left, center, center, center),
    table.header(
      [*Structure*],
      [#text(size: 10pt, $chevron.l sugar(+)sugar(2)sugar(2) , sugar(*)sugar(2)sugar(2) chevron.r$)],
      [#text(size: 10pt, $(lambda x . (lambda y . sugar(I)) Omega (sugar(+) x x) )(sugar(+) sugar(1) sugar(2))$)],
      [#text(size: 10pt, $sugar(P)sugar(8)$)]
    ),
    [Tas binaire], [0,0013s], [1m30s], [6m48s],
    [Tas binomial], [0,0012s], [16s], [1m31s],
    [Tas de Fibonacci], [0,0014s], [11s], [30s],
  ),
)


// ════════════════════════════════════════════════════════════════════════════
= Conclusion

Ce TIPE a formalisé la recherche de stratégie de réduction optimale comme un problème de
plus court chemin dans un graphe potentiellement infini, et a conduit au développement d'un
interpréteur OCaml complet mettant en œuvre A\* avec des heuristiques spécifiques au
lambda-calcul. Les résultats confirment un gain significatif (15 pas contre 21
sur l'exemple présenté). L'optimisation de l'algorithme ouvre des perspectives d'amélioration 
par le choix de la file de priorité et l'affinement des heuristiques afin d'être en capacité de
traiter des termes plus conséquent.

Finalement, malgré des optimisations permettant de réduire la taille des graphes explorés
et les complexités des opérations, de nombreux termes restent hors d'atteinte dans des limites de
temps et de mémoire raisonnables. Aussi il pourrait alors être intéressant de s'intéresser
au approximations de ces réductions minimales pour augmenter la portée de notre outil en
perdant une certaine part d'information.

#pagebreak()
= Annexe

#v(1cm)

Graphes illustrant l'optimisation sur l'ordre des dérivations d'un terme en forme normale de tête appliquée à $chevron.l sugar(+) sugar(2) sugar(2), sugar(*) sugar(2) sugar(2) chevron.r$
#figure(
  muchpdf(
    read("prunning_test_without.pdf", encoding: none),
  ),
  caption: "Sans optimisation"
)

#figure(
  muchpdf(
    read("prunning_test_with.pdf", encoding: none),
  ),
  caption: "Avec optimisation"
)

#pagebreak()
// ════════════════════════════════════════════════════════════════════════════
#bibliography("refs.yaml", style: "ieee", title: "Bibliographie")

