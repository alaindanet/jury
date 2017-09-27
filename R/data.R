#' Une composition de jury de thèse valide.
#'
#' Un jeu de donnée contenant un exemple de jury de thèse valide ainsi que
#' toutes les informations nécessaires à la vérification de la validité de sa
#' composition.
#' 
#'
#' @format A tibble avec 6 lignes and 5 variables:
#' \describe{
#'   \item{civilité}{civilité, monsieur ou madame}
#'   \item{rang}{A ou B, désignant respectivement directeur.es de
#'   recherche/professeur.es ou chargé.es de recherche/maitre.s de conférence}
#'   \item{hdr}{diplôme d'habilitation à diriger les recherches, oui, non, ou
#'   équivalence pour les chercheurs étrangers}
#'   \item{local}{rattachement à l'école doctorale du candidat, discriminant les
#'   membres du jury locaux et extérieur}
#'   \item{role}{rôle du membre du jury, soit directeur, encadrant, rapporteur
#'   ou examinateur}
#'
#' }
#' @source Exemple factice
"jury_example"
