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

## Vérifier la composition de son jury de thèse

```r
library(jury)
data(jury_example)
jury_check(jury_example)
```

## Combinaison de jury de thèse

En fournissant une liste de personne que vous souhaiteriez inviter dans votre
jury de thèse, la fonction `jury_check_all` vous retournera l'ensemble des
combinaison de jury de thèse valide.


```r
library(jury)
data(people_suggestion)
jury_check_all(people_suggestion)
```
