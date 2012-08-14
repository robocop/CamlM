# CamlM

Page d'accueil : http://robocop.github.com/CamlM/ .
CamlM est un langage de programmation basé sur le langage caml et étendu pour pouvoir faire facilement du calcul formel, de manière analogue à Maple ou Mathématica (du moins en théorie).

## Installation

Nécessite Linux ou Mac, ainsi qu'une installation de OCaml (=> 3.10) et de Menhir.

1. Faire une copie du dépot : `git clone git://github.com/robocop/CamlM.git`
2. Executer la commande : `./configure`
3. Executer la commande : `make`
4. Lancer l'interpreteur avec `./camlm` (nécessite rlwrap. On peut aussi utiliser emacs avec le tuareg-mode)

## Documentation

Executer la commander `make doc` pour générer la documentation.

## Développement

Si le fichier \_oasis est modifié, le regénérer avec la commande `oasis setup`
Le "back-end" du compilateur se trouve dans `src/compiler`. 
Le seul "front-end" actuellement disponible est le REPL, et se trouve dans `src/repl`.

## Exemples de programmes

CamlM supporte toutes les fonctionnalités de base de CamlM (fonctions récusives, filtrage, typage).

### Fonction factorielle
```Ocaml
# let rec fac = function 0 -> 1 | n -> n*fac(n-1) in fac 5;;
:- int = 120
```

### Dérivation d'expression mathématique : 

```Ocaml
# open Maths;;
:- unit = ()
# open Formel;;
:- unit = ()
# d (\x -> x*ln x);; (* la notation des fonctions anonymes est analogue à celle utilisée dans Haskell : \x -> f x *)
:- (int -> int) = \x -> (ln x + 1) (* la fonction est dérivée puis simplifiée automatiquement grâce à la fonction d présente dans maths.mml *)
```

### Autres exemples

Il suffit d'ouvrir les fichiers d'extension `.mml` qui contiennent du code écrit en CamlM.

## Sémantique du langage CamlM

Pour avoir une vue plus complète des fonctionnalités du langage, on pourra visiter cette page : [Éléments de sémantique du langage CamlM](https://github.com/robocop/CamlM/wiki/%C3%89l%C3%A9ments-de-s%C3%A9mantique-du-langage-CamlM). 
