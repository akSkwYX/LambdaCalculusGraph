= Bibliographie

[1] H.P. BARENDREGT, The lambda Calculus - Its Syntax and Semantics.

[2] JEAN GOUBAULT-LARRECQ, Lambda-calcul et langages fonctionnels.

[3] H.P. BARENDREGT, Needed Reduction and Spine Strategies for the Lambda Calculus.

[4] PETER SESTOFT, Demonstrating Lambda Calculus Reduction.

[5] JEAN-JACQUES LÉVY, Optimal Reductions in the lambda-calculus.

[6] WIKIPEDIA COMMUNITY, $A^*$ search algorithm.

[7] WIKIPEDIA COMMUNITY, D-ary heap

= Bibliographie commentée

Le lambda calcul est un modèle de calcul fondamental en informatique théorique. En effet c'est lui qui constitue la base des langages de programmation fonctionnels comme Haskell ou Ocaml. Un lambda terme représente un programme et la réduction de ce terme correspond à l'exécution du programme. Cette réduction de lambda termes est donc au coeur des langages de programmation fonctionnels. Cependant il existe plusieurs manières de réduire un lambda terme, et toutes ne sont pas équivalentes, notamment du point de vue de la complexité. Ainsi il semble important d'étudier les différentes stratégies de réduction possibles afin d'optimiser l'exécution des programmes fonctionnels.
La première étape est donc de pouvoir simuler la réduction d'un lambda terme, c'est le role de l'interpréteur. Il faut de plus qui soit modulable afin de pouvoir tester différentes stratégies de réduction.
A partir de cette simulation de réduction on peut créer le graphe des réductions possibles d'un lambda terme. Ce graphe permet ensuite d'analyser les différentes stratégies de réduction et de les comparer. La notion de stratégie optimale est alors étudiée, on parlera d'optimalité dans le sens du minimum de pas de réduction nécessaires pour atteindre une forme normale (correspondant à la fin de l'exécution du programme).
De cette volonté d'étude apparaisse deux problème principaux. Comment s'assurer que l'on atteint bien une forme normale et donc que notre programme d'analyse termine ? Et comment gérer les cas où le graphe des réductions est trop grand pour etre parcouru exhaustivement ? Les théorèmes sur le lambda calcul nous aident à répondre à la première question et assure que notre programme termine dans une certaine classe de lambda termes. Pour la seconde question, c'est la recherche d'algorithme de recherche de plus cours chemin dans un graphe et l'étude d'heuristiques adapté au lambda termes qui constituerons la partie principale de ce travail.

= Reformulation par Gemini

Le lambda-calcul constitue un modèle de calcul fondamental en informatique théorique.
Il sert de fondement théorique aux langages de programmation fonctionnels tels que Haskell ou OCaml.
Dans ce paradigme, un lambda-terme est assimilé à un programme, et l’exécution de ce dernier se traduit par la $beta$-réduction du terme [1][2].
Toutefois, un même terme peut être réduit de multiples manières ; ces stratégies de réduction ne sont pas équivalentes et présentent des disparités notables, notamment en termes de complexité algorithmique [1][2].
L’étude des différentes stratégies s'avère donc cruciale pour optimiser l'exécution des programmes fonctionnels.
La première étape de ce travail consiste à simuler la réduction d’un lambda-terme via le développement d'un interpréteur tel que [4].
Celui-ci doit être conçu de manière modulable pour permettre l’expérimentation et le test de diverses règles de réduction.
À partir de ces simulations, il devient possible de construire le graphe des réductions possibles d’un terme, outil essentiel pour analyser et comparer les trajectoires de calcul.
Dans ce contexte, une stratégie est dite « optimale » lorsqu'elle minimise le nombre de pas de réduction requis pour atteindre une forme normale, ce qui correspond à la fin de l’exécution du programme [5].
Cette volonté d’analyse soulève deux problématiques majeures. 
La première concerne la terminaison : il s'agit de garantir que le programme d'analyse atteint bien une forme normale.
Les théorèmes fondamentaux du lambda-calcul apportent des éléments de réponse en assurant la terminaison pour certaines classes de termes [1][2].
La seconde difficulté réside dans la gestion de graphes de réduction dont la taille peut interdire un parcours exhaustif.
Pour pallier cette explosion combinatoire, ce travail se concentre sur l’application d’algorithmes de recherche de plus court chemin, tels que l’algorithme $A^*$[6].
L'étude d'heuristiques spécifiques aux lambda-termes [3] et l'utilisation de structures de données optimisées, comme les tas d-aires [7], constituent ainsi le cœur de cette recherche.
