#' Vérifier la validité des données
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
#' @return Retourne un tibble contenant le jeu de données si ce dernier est
#' valide.
#'
#' @seealso jury_check 
#'
#' @export
jury_validity <- function(data){

    ## Check data class
    #if(is.null(data)) stop("Veuillez fournir une composition de jury!")
    stopifnot(any(class(data) %in% c("matrix","list", "data.frame")))

    colnames(data)  <- stringr::str_to_lower(colnames(data))
    data <- apply(data, 2, function(x) stringr::str_to_lower(x))
    data <- tibble::as_tibble(data)

    var_needed <- c("civilité", "rang", "hdr", "local", "role")

    if( any(!(var_needed %in% colnames(data))) ){
	stop("data doit au moins contenir les variables suivantes: civilité, rang, hdr, local, role")}

    # check variable
    if( any(!(data$civilité %in% c("mme", "m"))) ) {
	stop("La civilité fournie ne peut être que Mme ou M (non sensible à la casse).")}

    if( any(!(data$rang %in% c("a", "b"))) ) {
	stop("Le rang fourni ne peut être que A ou B (non sensible à la casse)")}

    if( any(!(data$hdr %in% c("oui", "non", "équivalent"))) ) {
	stop("Le hdr fourni ne peut être que Oui, Non ou Équivalent (non sensible à la casse)")}

    if( any(!(data$local %in% c("local", "extérieur"))) ) {
	stop("La localité fournie ne peut être que Local ou Extérieur (non sensible à la casse)")}

    if( any(!(data$role %in% c("rapporteur", "examinateur", "directeur", "encadrant"))) ) {
	stop("Le role fourni ne peut être que Rapporteur, Directeur, Encadrant ou Examinateur 
	    (non sensible à la casse)")}

	    return(data)
    }

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
#' @seealso jury_check_all 
#'
#' @return retourne une erreur si data n'est pas dans un bon format ou si la
#' composition du jury de thèse n'est pas valide.
#'
#' @examples
#' data(jury_example)
#' jury_check(jury_example)
#'
#' @export
jury_check <- function(data, binary = FALSE){

    

    require(magrittr) #needed to use pipe
    data %<>% jury_validity()

    # Check validité du jury


    if(binary == FALSE){
	mystop <- stop
	mymessage <- message 
	mywarning <- warning
    }else{
	mystop <- function(x, ...) {
	    return(FALSE)
	}
	mymessage <- function(x,...) {
	    return(TRUE)
	} 
	mywarning <- function(x,...) {
	}
	result <- TRUE # Par défaut le jury est bon

    }

    ## Nombre de rapporteur, d'examinateur et directeur de thèse 
    if(nrow(data) < 5) {
	result <- mystop("Le jury doit être composé de cinq membres au minimum: deux rapporteurs, deux
	    examinateurs et un directeur de thèse")}
    if(nrow(data) > 6) {
	result <- mystop("Le jury doit être composé de six membres au maximum si on exclue les invités.")}
    ## Nombre de rapporteurs
    if( dplyr::filter(data, role == "rapporteur") %>% dplyr::summarise(n()) %>% unlist < 2 ) {
	result <- mystop("Le jury doit être composé de deux rapporteurs au minimum")}
    ## Nombre d'examinateurs
    if( dplyr::filter(data, role == "examinateur") %>% dplyr::summarise(n()) %>% unlist < 2 ) {
	result <- mystop("Le jury doit être composé de deux examinateurs au minimum")}
    ## Nombre de rang A
    if( length(which(data$rang == "a"))/nrow(data) < 1/2 ) {
	result <- mystop("La moitié des membres jury au minimum doit être de rang A")}
    ## Localité
    if( length(which(data$local == "extérieur"))/nrow(data) < 1/2 ) {
	result <- mystop("La moitié des membres jury au minimum doit être rattaché à une école doctorale extérieure")}
    ## Rapporteur et HDR
    if( any(data$role == "rapporteur" & !(data$hdr %in% c("oui", "équivalent")) ) ) {
	result <- mystop("Les rapporteurs doivent obligatoirement posséder l'HDR ou l'équivalence")}
    ## Directeur et HDR
    if( any(data$role == "directeur" & data$hdr != "oui") ) {
	result <- mystop("Les directeurs doivent obligatoirement posséder l'HDR")}

    ## Parité 
    genre_ratio <- length(which(data$civilité == "mme"))/nrow(data) %>% round(., 1)
    if( genre_ratio != 1/2 ) {
	mywarning("La composition du jury doit assurer une représentation équilibrée de femmes et d'hommes.\n",
	    "La composition que vous avez proposé sous-représente les ",
	    ifelse(genre_ratio < 1/2, "femmes", "hommes"),"." )}

    ## Tout a l'air parfait!
   mymessage("La composition de jury de thèse que vous avez proposé semble valide.\n",
	"Il est néanmoins nécessaire d'obtenir confirmation auprès de votre école doctorale.")

   if(binary == TRUE){
       if(result == FALSE){ return(FALSE)}else{return(TRUE)}
   }

    }

