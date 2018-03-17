#' Vérifier la validité de la composition de son jury de thèse.
#' 
#' @param data un data.frame ou une matrice contenant les variables civilité, rang,
#' local, hdr, role 
#'
#' @details le contenu des variables civilité, rang, local, hdr et role est
#' insensible à la casse. Cependant, les valeurs que peuvent prendre ces
#' variables sont strictement contraintes. La variable civilité ne peut prendre que
#' la valeur Mr ou Mme; la variable rang ne peut prendre que les valeurs A ou B
#' désignant respectivement directeur.es de recherche/professeur.es ou chargé.es
#' de recherche/maitre.s de conférence; la variable local ne peut prendre que
#' les valeurs local ou extérieur, indiquant si les membres du jury sont
#' rattachés ou non à l'école doctorale dans laquelle est inscrit le doctorant;
#' La variable hdr ne peut prendre que les valeurs Oui, Non ou Équivalent,
#' indiquant si les membres du jury possèdent ou non le diplôme d'habilitation à
#' diriger les recherches; la variable role ne peut prendre que les valeurs
#' Directeur, Encadrant, Examinateur et Rapporteur.  
#' 
#' @seealso jury_check
#'
#' @return retourne une liste contenant l'ensemble des combinaisons de membres
#' du jury qui donne une composition de jury de thèse valide.
#'
#' @examples
#' data(people_suggestion)
#' jury_check_all(people_suggestion)
#'
#' @export

#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"

jury_check_all <- function(data, n = NULL){

    if(is.null(n)){
	n <- 5
    }
    data %<>% jury_validity()

    # n doit être supérieur à 5 et être au maximum égal à 6
    if(!(n %in% c(5, 6))){
	stop("n ne peut que prendre les valeurs 5 et 6.")
    }

    # List of all possible combinations
    possible <- combn(1:nrow(data), n,
	FUN = function(x) dplyr::slice(data, x),
	simplify = FALSE)

    valide <- lapply(possible, jury_check, binary = TRUE)
    result <- possible[which(valide == TRUE)]

    result %<>% list(result = ., nbcombination = length(valide))


    class(result) <- "jury_list"
    result

}
