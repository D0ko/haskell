# Projets Haskell

Ce dépôt contient trois implémentations de différents algorithmes et structures de données en Haskell.

## 📋 Table des matières

1. [Rush Hour](#rush-hour)
2. [Countdown](#countdown)
3. [MTree](#mtree)
4. [Installation et exécution](#installation-et-exécution)

## Rush Hour

Implémentation du jeu de puzzle Rush Hour en Haskell. Le but est de faire sortir un véhicule spécial d'un plateau encombré d'autres véhicules.

### Fonctionnalités
- Modélisation des véhicules (voitures et camions) et de la grille de jeu
- Représentation visuelle du plateau avec caractères ASCII
- Algorithme de recherche de solution (BFS)
- Plusieurs niveaux prédéfinis de difficulté variable
- Gestion des mouvements légaux des véhicules

### Structure du code
- `Vehicle`: représentation des véhicules avec leurs positions
- `Grid`: représentation du plateau de jeu
- `Move`: représentation des mouvements possibles
- `Path`: séquence de mouvements avec la grille résultante
- Fonctions utilitaires pour déterminer les mouvements légaux et rechercher une solution

## Countdown

Implémentation du problème du "Countdown", un jeu mathématique où le but est d'atteindre un nombre cible en utilisant des opérations arithmétiques sur un ensemble de nombres donnés.

### Fonctionnalités
- Expressions arithmétiques avec opérateurs binaires (addition, soustraction, multiplication, division)
- Vérification de la légalité des opérations (ex: divisions sans reste)
- Algorithme de recherche pour trouver la meilleure solution
- Génération de toutes les expressions possibles à partir d'une liste de nombres

### Structure du code
- `AExpr`: représentation des expressions arithmétiques
- `BOp`: opérateurs binaires (Add, Sub, Mul, Div)
- `VAExpr`: expression avec sa valeur calculée
- Fonctions pour générer et évaluer les expressions arithmétiques

## MTree

Bibliothèque pour manipuler des arbres multi-branches (MTree) en Haskell avec diverses fonctions utilitaires.

### Fonctionnalités
- Création et manipulation d'arbres généraux
- Parcours en profondeur et en largeur
- Fonctions de comptage, recherche, transformation et filtrage
- Opérations de pliage (fold) pour arbres généraux
- Extraction de chemins et calcul de signatures
- Affichage formaté des arbres

### Structure du code
- `MTree`: structure de données pour les arbres multi-branches
- `MForest`: liste d'arbres multi-branches
- Fonctions utilitaires comme:
  - `mTreeCount`: nombre de nœuds
  - `mTreeLeaves`: liste des feuilles
  - `mTreeToList`: conversion en liste
  - `mTreeHeight`: calcul de la hauteur
  - `mTreeDepthFirstTraversal`: parcours en profondeur
  - `mTreeBreadthFirstTraversal`: parcours en largeur
  - `mTreeMap`: application d'une fonction à chaque nœud
  - et plus encore...

## Installation et exécution

Pour utiliser ces modules:

1. Assurez-vous d'avoir GHC (Glasgow Haskell Compiler) et Cabal installés sur votre système.
2. Chargez un module dans GHCi:
```bash
ghci rushhour.hs
ghci countdown.hs
ghci MTree-skel.hs
```
3. Testez les fonctions:
```haskell
-- Pour Rush Hour
toString grid1
legalMoves grid1

-- Pour Countdown
countdown1 100 [1, 3, 7, 10, 25, 50]

-- Pour MTree
mTreeExample
mTreeToList mTreeExample
mTreeHeight mTreeExample
```

---

*Ces projets ont été réalisés dans le cadre d'un cours de programmation fonctionnelle en Haskell.*

## Membres

Quentin Benesby
Sophie Cousson