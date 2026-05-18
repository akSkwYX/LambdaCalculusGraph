#import "@preview/touying:0.7.3": *
#import themes.university: *
#import "@preview/theorion:0.6.0": *
#import cosmos.clouds: *

#show: show-theorion

#show: university-theme.with(
  aspect-ratio: "4-3",
  align: horizon,
  config-info(
    title: [Optimisation de la réduction d'expressions du lambda calcul],
    author: [Hurot Eliott],
    date: [2025-2026],
    logo: [$lambda$]
  )
)

#let eqalpha = math.class("relation", math.attach(sym.eq, br: sym.alpha))

#set text(30pt)
#set align(center)
#show math.equation: set text(30pt)

#title-slide()

== Sommaire <touying:hidden>

#components.adaptive-columns(outline(title: none, indent: 1em))

= Définitions

== Syntaxe & Sémantique

$ Lambda 
    &:= x &text("Variable", fill: #color.hsl(0deg, 0, 45%)) \
    &bar M N &text("Application", fill: #color.hsl(0deg, 0, 45%))\
    &bar lambda x . M &text("Fonction", fill: #color.hsl(0deg, 0, 45%)) $
\
$ M, N in Lambda text("et") x in V $
\
Rédex : $display((lambda x . M) N)$

== Réductions

#grid(
  align: top,
  columns: (auto, auto),
  column-gutter: 3cm,
  rows: 1,
[
#set text(20pt)
#show math.equation: set text(20pt)
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
$ cases(delim: "|", reverse: #false, u_1 eqalpha u_2, v_1 eqalpha v_2) &arrow.double u_1 v_1 eqalpha u_2 v_2 \
  u eqalpha v &arrow.double lambda x . u eqalpha lambda x . v $
]
)

= Terminaison et confluence

== Terminaison

#set text(25pt)
#show math.equation: set text(25pt)

_Forme normale_ : on ne peut plus simplifier #sym.arrow.double Unicité / Existence ? \
La $beta$ réduction ne termine pas toujours : $Omega := (lambda x . x x)(lambda x .x x)$
$ Omega &arrow (x x)[x := (lambda x . x x)]
        &arrow (lambda x . x x)(lambda x . x x) = Omega $
_Fortement normalisant_ : LES réductions se terminent \
_Faiblement normalisant_ : UNE Réduction se termine
$ (lambda x . y) Omega $

== Confluence

#theorem[Confluence du lambda calcul][
  Si $u arrow^* v_1$ et $u arrow^* v_2$, alors il existe $w$ tel que $v_1 arrow^* w$ et $v_2 arrow^* w$
]

#corollary[Unicité des formes normales][
  Si $u_1$ et $u_2$ sont deux formes normales de $u$, alors $u_1 = u_2$
]

= Stratégies de réductions

== Stratégies internes


