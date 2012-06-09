# CamlM

CamlM est un langage de programmation basé sur le langage caml et étendu pour pouvoir faire facilement du calcul formel.

## Installation

Nécessite Linux ou Mac, ainsi qu'une installation de OCaml (=> 3.10) et de Menhir.

1. Faire une copie du dépot : `git clone git://github.com/robocop/CamlM.git`
2. Créer le dossier build/
3. Executer la commande : `/.makefile.sh`


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
# d (\x -> x*ln x);;
:- (int -> int) = \x -> (ln x + 1)
```

### Autres exemples

Il suffit d'ouvrir les fichiers d'extension `.mml` qui contiennent du code écrit en CamlM.

## Sémentique du langage CamlM

Pour avoir une vue plus complète des fonctionnalités du langage, on pourra visiter cette page : [Éléments de sémantique du langage CamlM](https://github.com/robocop/CamlM/wiki/%C3%89l%C3%A9ments-de-s%C3%A9mantique-du-langage-CamlM). 
