# Pourquoi jury ?

Parce qu'il est assez difficile de s'y retrouver dans les règles de composition
du jury de thèse de l'Université de Montpellier, voici un petit paquet qui vous
permettra de vérifier la validité de sa composition.

La motivation première de la création de ce package était de se faire la main
sur la fabrication d'un package.

# Installation

```r
library(devtools)
install_github("alaindanet/jury", build_vignettes=TRUE)
```

# Usage

## Consulter la vignette

```r
browseVignettes("jury")
```

## Format de données d'entrée

Les fonctions de `jury` sont insensibles à la casse et aux accents, donc vous
pouvez écrire avec des accents ou non.

```r
civilité <- c("Mme", "Mme", "M", "M", "Mme", "M")
rang <- c("A", "A", "B", "B", "A", "B")
hdr <- c("Oui", "Oui", "Équivalent", "Non", "Oui", "Non")
local <- c("Extérieur", "Local", "Extérieur", "Extérieur", "Local", "Local")
role <- c("Rapporteur", "Examinateur", "Rapporteur", "Examinateur", "Directeur", "Encadrant")

jury <- tibble::as_tibble(cbind(civilité, rang, hdr, local, role))
```

Ce sont les seules variables dont `jury` a besoin. Cependant, il est recommandé d'ajouter le nom (ou prénoms, surnoms, ...) des membres du jury pour s'y retrouver,   

```r
prénoms <- c("Laure", "Artémis", "Akim", "Pierre", "Yasmine", "Malo")
jury <- cbind(jury, prénoms) 
```


## Vérifier la composition de son jury de thèse

```r
library(jury)
data(jury_example)
str(jury_example)
jury_check(jury_example)
```

## Combinaison de jury de thèse

En fournissant une liste de personnes que vous souhaiteriez inviter dans votre
jury de thèse, la fonction `jury_check_all` vous retournera l'ensemble des
combinaisons de jury de thèse valides.


```r
library(jury)
data(people_suggestion)
str(people_suggestion)
valid_jury <- jury_check_all(people_suggestion)
```

On peut consulter la liste des compositions de jury compatibles. Ces
compositions sont affichées par ordre décroissant de la somme des notes de
préférences des membres de jury. On peut contrôler le nombre de composition à
afficher avec l'argument `n`.

```r
print(valid_jury, n = 3)
```

## TODO

- [x] Autoriser l'écriture sans accent!
- [ ] Autoriser la spécification des invités (n <= 2)
- [x] Autoriser une longueur de jury supérieur à 6 check_nb_row et jusqu'à 8
(sans les invités)
