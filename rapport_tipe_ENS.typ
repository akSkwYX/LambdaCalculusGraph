// ── Paramètres globaux ──────────────────────────────────────────────────────
#set document(title: "Optimisation de la réduction d'expressions du lambda-calcul")
#set page(
  paper: "a4",
  margin: (top: 2.5cm, bottom: 2.5cm, left: 2.5cm, right: 2.5cm),
  numbering: "1",
  number-align: center,
)
#set text(font: "New Computer Modern", size: 11pt, lang: "fr")
#set par(justify: true, leading: 0.65em)
#set heading(numbering: "1.")
#show heading: it => {
  v(0.6em, weak: true)
  it
  v(0.3em, weak: true)
}

// ── Environnements théorèmes maison ──────────────────────────────────────────
#let thm-counter = counter("theorem")
#let def-counter = counter("definition")

#let thmblock(title, body, fill: rgb("#eef3fb"), stroke-color: rgb("#3a5fa0"), label: none) = {
  thm-counter.step()
  block(
    width: 100%,
    fill: fill,
    stroke: (left: 3pt + stroke-color),
    inset: (left: 10pt, right: 8pt, top: 6pt, bottom: 6pt),
    radius: (right: 3pt),
  )[
    #text(weight: "bold")[#title #context thm-counter.display("1").] #h(0.5em) #body
  ]
}

#let defblock(title, body, fill: rgb("#edfbf0"), stroke-color: rgb("#3a8a50")) = {
  def-counter.step()
  block(
    width: 100%,
    fill: fill,
    stroke: (left: 3pt + stroke-color),
    inset: (left: 10pt, right: 8pt, top: 6pt, bottom: 6pt),
    radius: (right: 3pt),
  )[
    #text(weight: "bold")[#title #context def-counter.display("1").] #h(0.5em) #body
  ]
}

#let corblock(body) = {
  block(
    width: 100%,
    fill: rgb("#fff8ee"),
    stroke: (left: 3pt + rgb("#b07a20")),
    inset: (left: 10pt, right: 8pt, top: 6pt, bottom: 6pt),
    radius: (right: 3pt),
  )[
    #text(weight: "bold")[Corollaire.] #h(0.5em) #body
  ]
}

#let proofblock(title: "Démonstration", body) = {
  block(
    width: 100%,
    inset: (left: 12pt, right: 8pt, top: 4pt, bottom: 4pt),
  )[
    #text(style: "italic")[#title.] #h(0.4em) #body #h(1fr) $square$
  ]
}

#let sketchblock(body) = {
  block(
    width: 100%,
    inset: (left: 12pt, right: 8pt, top: 4pt, bottom: 4pt),
  )[
    #text(style: "italic")[Démonstration (esquisse).] #h(0.4em) #body #h(1fr) $square$
  ]
}

// ── Raccourcis mathématiques ─────────────────────────────────────────────────
#let bred = $arrow.r_beta$
#let breds = $arrow.r^*_beta$
#let bpar = $arrow.r_parallel$
#let sugar(x) = $ceil x ceil.r$


    Le lambda-calcul constitue un modèle de calcul universel équivalent aux machines de Turing. Un même terme peut être réduit vers sa forme normale par de multiples chemins, dont les longueurs diffèrent considérablement. Ce TIPE implémente un interpréteur de lambda-termes en OCaml, construit paresseusement le graphe orienté des réductions possibles, et applique l'algorithme A\* muni d'heuristiques admissibles spécifiques aux lambda-termes pour déterminer la séquence de réduction minimale.

#pagebreak()
#outline(indent: 1.5em, depth: 2)
#pagebreak()

// ════════════════════════════════════════════════════════════════════════════
= Introduction

Le lambda-calcul, introduit par Alonzo Church dans les années 1930, formalise la notion
de calcul par application et substitution de variables. Il constitue le fondement théorique
des langages fonctionnels modernes tels que Haskell ou OCaml @barendregt @goubault,
dans lesquels l'exécution d'un programme correspond précisément à la _béta-réduction_ d'un
lambda-terme.

Une propriété fondamentale de ce modèle est que la forme normale d'un terme — son résultat
de calcul — est unique lorsqu'elle existe (§2.2). En revanche, les _chemins_ pour l'atteindre
sont multiples, et leur longueur peut varier considérablement. Par exemple, le terme
$(lambda x.(lambda x.[I])sugar(Omega)(sugar(+)x x)(sugar(+)sugar(1)sugar(2))$ admet un chemin optimal
en *15 pas*, tandis que la stratégie externe gauche — pourtant garantie de terminer — en
requiert *21* et la stratégie interne gauche ne termine même pas.
#v(0.3em)
*Problématique.* Comment appliquer la recherche de plus court chemin dans un graphe
à la détermination de stratégies de réduction optimales pour les lambda-termes ?
#v(0.3em)

L'objectif est triple : (1) implémenter un interpréteur supportant diverses stratégies ;
(2) construire le graphe de réductions de manière paresseuse ; (3) déterminer la séquence
minimale par A\* et analyser les heuristiques admissibles associées.

// ════════════════════════════════════════════════════════════════════════════
= Fondements théoriques

== Syntaxe et réductions

#defblock("Définition")[
  L'ensemble $Lambda$ des *lambda-termes* est défini inductivement par :
  $ Lambda ::= x quad bar.v quad M N quad bar.v quad lambda x . M $
  où $x$ parcourt un ensemble dénombrable de variables $V$, $M N$ est l'_application_ de $M$
  à $N$, et $lambda x . M$ est l'_abstraction_ (liaison de $x$ dans $M$).
  Un sous-terme de la forme $(lambda x . u)\,v$ est appelé un *redex*.
]

L'*alpha-équivalence* $=_alpha$ identifie les termes ne différant que par le renommage
cohérent de variables liées : $lambda x . x =_alpha lambda y . y$. On travaille dans le
quotient $Lambda / {=_alpha}$, rendu décidable par l'encodage de De Bruijn (§3.1).

#defblock("Définition")[
  La *béta-réduction* $bred$ est la plus petite relation sur $Lambda/{=_alpha}$ vérifiant :
  $ (lambda x . u)\,v bred u[x := v] $
  et stable par contexte (application et abstraction). Sa clôture réflexive-transitive est
  notée $breds$. Un terme sans redex est appelé *forme normale*.
]

La *béta-réduction ne termine pas en général* : le terme $Omega := (lambda x . x x)(lambda x . x x)$
satisfait $Omega bred Omega$ et n'a pas de forme normale.

== Terminaison et confluence

On distingue deux niveaux de normalisation :
- *Fortement normalisant (SN)* : toutes les suites de réductions de $u$ sont finies.
- *Faiblement normalisant (WN)* : il existe au moins une suite finie menant à une forme normale.

#thmblock("Théorème", fill: rgb("#eef3fb"), stroke-color: rgb("#3a5fa0"))[
  *(Confluence — Church-Rosser)* Si $u breds v_1$ et $u breds v_2$, alors il existe $w$
  tel que $v_1 breds w$ et $v_2 breds w$.
]

#proofblock[
  On introduit la *réduction parallèle* $bpar$, qui contracte simultanément un ensemble de
  redex disjoints. Elle satisfait $bred space subset.eq space bpar space subset.eq space breds$.
  L'étape clé est la *propriété du diamant pour $bpar$* : si $u bpar v_1$ et $u bpar v_2$,
  il existe $w$ tel que $v_1 bpar w$ et $v_2 bpar w$. Ce $w$ s'obtient en contractant dans
  $v_1$ (resp. $v_2$) les redex créés par la réduction vers $v_2$ (resp. $v_1$) ; la
  compatibilité des substitutions parallèles assure la cohérence. La confluence de $breds$
  s'obtient alors par induction sur la longueur des séquences @barendregt [ch. 3].
]

#corblock[
  La forme normale d'un terme, lorsqu'elle existe, est *unique à $=_alpha$ près*.
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

#thmblock("Théorème", fill: rgb("#eef3fb"), stroke-color: rgb("#3a5fa0"))[
  *(Standardisation)* Si $u$ est faiblement normalisant, la stratégie externe gauche calcule
  la forme normale de $u$ en un nombre fini de pas.
]

#sketchblock[
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

// ════════════════════════════════════════════════════════════════════════════
= Implémentation de l'interpréteur

== Représentation par indices de De Bruijn

La gestion de l'alpha-équivalence est cruciale : sans elle, des termes identiques seraient
traités comme distincts dans le graphe de réductions, causant des explosions combinatoires.
Les *indices de De Bruijn* remplacent chaque variable liée par sa _profondeur de liaison_ :

#align(center)[
  $lambda x . lambda y . x\,y quad arrow.r.long quad lambda . lambda . 1 space 0$
]

Avec cet encodage, l'alpha-équivalence devient l'*égalité syntaxique*, ce qui permet une
comparaison de termes en $O(n dot v)$ (où $n$ est la taille de l'arbre et $v$ la profondeur
maximale des variables liées) — décisive pour détecter les sommets déjà visités dans le graphe.

== Pipeline de traitement

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
    [1], [Chaîne $arrow.r$ Tokens], [linéaire],
    [2], [Tokens $arrow.r$ AST], [linéaire],
    [3], [AST $arrow.r$ Graphe (paresseux)], [$O(n^3)$ par sommet],
    [4], [Graphe $arrow.r$ Plus court chemin], [A\*, guidé par $h$],
  ),
  caption: [Pipeline de traitement],
)

La construction du graphe (étape 3) est *paresseuse* : les successeurs d'un nœud ne sont
calculés que lors de son extraction de la file de priorité. Cette approche est indispensable
car le graphe peut être infini (terme divergent) ou d'une taille rédhibitoire
($sugar(P)sugar(10)$ : 310 000 arêtes).

Les complexités des opérations élémentaires (avec $n$ = nombre de nœuds, $m$ = taille du terme
substitué) sont :

#figure(
  table(
    columns: (1fr, auto),
    stroke: (x, y) => if y == 0 { (bottom: 0.8pt) } else { (bottom: 0.4pt) },
    inset: (x: 8pt, y: 5pt),
    table.header([*Opération*], [*Complexité*]),
    [Égalité structurelle (De Bruijn)], [$O(n dot v)$],
    [Substitution de $x$ par $v$ dans $u$], [$O(n + m^2)$],
    [Étape de réduction complète], [$O(n^3)$],
  ),
  caption: [Complexités des opérations critiques],
)

// ════════════════════════════════════════════════════════════════════════════
= Recherche du chemin optimal par A\*

== Formulation comme problème de plus court chemin

Le *graphe de réductions* $G_u$ d'un terme $u$ est le graphe orienté dont les sommets sont
les termes accessibles depuis $u$ par $breds$, et les arêtes sont les béta-réductions
élémentaires (chacune de poids 1). La forme normale de $u$, si elle existe, est l'unique
puits de ce graphe (par confluence). Le problème est donc : trouver le chemin de poids
minimal de $u$ vers sa forme normale dans $G_u$.

== L'algorithme A\* et admissibilité

#thmblock("Théorème", fill: rgb("#eef3fb"), stroke-color: rgb("#3a5fa0"))[
  *(Optimalité de A\*)* L'algorithme A\* retourne un plus court chemin de la source
  vers le but si et seulement si l'heuristique $h$ est *admissible* :
  $forall u, h(u) <= d^*(u)$, où $d^*(u)$ est le vrai nombre minimum de réductions
  pour normaliser $u$.
]

A\* sélectionne à chaque étape le nœud minimisant $f(u) = g(u) + h(u)$, où $g(u)$ est le
nombre de réductions déjà effectuées depuis le terme initial. L'exploration est *paresseuse* :
les successeurs ne sont générés que lorsqu'un nœud est extrait. Cela permet d'opérer sur
un graphe effectivement infini, sous réserve que A\* converge.

La supériorité de A\* sur Dijkstra est illustrée par le terme
$(lambda x.sugar(+)\, x\, x)(sugar(+)\, sugar(1)\, sugar(2))$ :

#figure(
  table(
    columns: (1fr, auto, auto),
    stroke: (x, y) => if y == 0 { (bottom: 0.8pt) } else { (bottom: 0.4pt) },
    inset: (x: 8pt, y: 6pt),
    align: (left, center, center),
    table.header([*Méthode*], [*Pas de réduction*], [*Optimalité garantie*]),
    [Stratégie interne], [$> 19$], [Non],
    [Stratégie externe gauche], [$19$], [Non],
    [A\* (heuristique $h_2$)], [$13$], [Oui],
  ),
  caption: [Comparaison sur $(lambda x.sugar(+) x x)(sugar(+)sugar(1)sugar(2))$],
)

_(Les graphes de réduction correspondants seront fournis en illustration.)_

== Heuristiques admissibles pour le lambda-calcul

Trois heuristiques ont été développées et étudiées @needed :

*$h_1$ — Taille du terme.* $h_1(u) = $ nombre de nœuds de l'AST de $u$. Intuitivement,
chaque réduction tend à simplifier le terme, mais une substitution peut dupliquer des
sous-termes et _augmenter_ la taille. $h_1$ peut donc sur-estimer $d^*(u)$ et n'est
*pas admissible* en général.

*$h_2$ — Nombre de redex.* $h_2(u) = $ nombre de redex dans $u$. Chaque pas contracte
un redex, mais peut en créer de nouveaux. L'admissibilité requiert que tout terme à $k$
redex soit normalisable en au moins $k$ pas — ce qui n'est pas garanti si chaque réduction
peut créer plusieurs redex supplémentaires.

*$h_3$ — Nombre de spine redex.* La _stratégie de la colonne vertébrale_ de Barendregt
@needed réduit en priorité les redex sur le chemin principal (spine) du terme.
$h_3(u) = $ nombre de spine redex constitue une heuristique plus informée. Son admissibilité
découle de la théorie des _réductions nécessaires_ : tout chemin vers la forme normale doit
contracter au moins une fois chaque spine redex.

#figure(
  table(
    columns: (auto, auto, 1fr),
    stroke: (x, y) => if y == 0 { (bottom: 0.8pt) } else { (bottom: 0.4pt) },
    inset: (x: 8pt, y: 6pt),
    align: (left, center, left),
    table.header([*Heuristique*], [*Admissible*], [*Remarque*]),
    [$h_1$ : taille], [Non], [Peut sur-estimer par duplication],
    [$h_2$ : \# redex], [À vérifier], [Dépend du terme],
    [$h_3$ : \# spine redex], [Oui (conjecture)], [Lié aux réductions nécessaires],
    [$h = 0$ (Dijkstra)], [Oui], [Triviale, non informative],
  ),
  caption: [Admissibilité des heuristiques étudiées],
)

== Structures de données pour la file de priorité

L'efficacité de A\* repose sur celle de la file de priorité (extraction du minimum et
insertion). On compare empiriquement plusieurs structures sur deux termes de référence :

#figure(
  table(
    columns: (1fr, auto, auto, auto),
    stroke: (x, y) => if y == 0 { (bottom: 0.8pt) } else { (bottom: 0.4pt) },
    inset: (x: 8pt, y: 6pt),
    align: (left, center, center, center),
    table.header(
      [*Structure*],
      [$sugar(P)sugar(5)$],
      [$sugar(P)sugar(10)$],
      [$sugar("fact")sugar(1)$],
    ),
    [Tas binaire], [0,012 s], [3 min 44 s], [—],
    [Tas binomial], [0,012 s], [3 min 25 s], [—],
    [Tas 2-3], [—], [—], [—],
    [Tas de Fibonacci], [—], [—], [—],
    [Pairing heap], [—], [—], [—],
  ),
  caption: [Temps d'exécution selon la structure de file de priorité (résultats partiels)],
)

Les complexités théoriques divergent : le tas de Fibonacci offre $O(1)$ amorti en
insertion et $O(log n)$ en extraction, contre $O(log n)$ pour le tas binaire dans les
deux cas @dheap. L'avantage pratique dépend toutefois de la localité mémoire
et des constantes cachées, d'où la nécessité du benchmark expérimental.

// ════════════════════════════════════════════════════════════════════════════
= Conclusion

Ce TIPE a formalisé la recherche de stratégie de réduction optimale comme un problème de
plus court chemin dans un graphe potentiellement infini, et a conduit au développement d'un
interpréteur OCaml complet mettant en œuvre A\* avec des heuristiques spécifiques au
lambda-calcul. Les résultats préliminaires confirment un gain significatif (13 pas contre 19
sur l'exemple présenté), avec des perspectives d'amélioration par le choix de la file de
priorité et l'affinement des heuristiques.

Les questions ouvertes portent sur la caractérisation précise des termes pour lesquels $h_2$
et $h_3$ sont admissibles, le lien avec la théorie des *réductions optimales* de Lévy
@levy, et l'éventuelle parallélisation de l'exploration.

// ════════════════════════════════════════════════════════════════════════════
#bibliography("refs.yaml", style: "ieee", title: "Bibliographie")
