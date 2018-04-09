#' Fonctions de vérification
#'
#' @param data
#'
#' @export
check_supervisor <- function(data, f) {
  result <- TRUE
  if( !any(data$role == "directeur") ) {
    result <- f("Le jury doit avoir un directeur de thèse.")
  }

  if( any(data$role == "directeur" & data$hdr != "oui") ) {
    result <- f("Les directeurs doivent obligatoirement posséder l'HDR")
  }
  return(result)
}
check_nb_row <- function(data, f) {
  result <- TRUE
  if(nrow(data) < 5 | nrow(data) > 6) {
    result <- f("Le jury doit être composé de cinq membres au minimum: deux
      rapporteurs, deux examinateurs et un directeur de thèse. Le jury doit être
      composé de six membres au maximum si on exclue les invités.") 
  }
  return(result)
}
check_nb_reviewer <- function(data, f) {
  result <- TRUE
  if( dplyr::filter(data, role == "rapporteur") %>% dplyr::summarise(n()) %>% unlist < 2 ) {
    result <- f("Le jury doit être composé de deux rapporteurs au minimum")
  }
  return(result)
}
check_nb_exam <- function(data, f) {
  result <- TRUE
  if( dplyr::filter(data, role == "examinateur") %>% dplyr::summarise(n()) %>% unlist < 2 ) {
    result <- f("Le jury doit être composé de deux examinateurs au minimum")
  }
  return(result)
}
check_rank <- function(data, f) {
  result <- TRUE
  if( length(which(data$rang == "a"))/nrow(data) < 1/2 ) {
    result <- f("La moitié des membres jury au minimum doit être de rang A")
  }
  return(result)
}
check_dom <- function(data, f) {
  result <- TRUE
  if( length(which(data$local == "extérieur"))/nrow(data) < 1/2 ) {
    result <- f("La moitié des membres jury au minimum doit être rattaché à une école doctorale extérieure")
  }
  return(result)
}
check_hdr_reviewer <- function(data, f) {
  result <- TRUE
  if( any(data$role == "rapporteur" & !(data$hdr %in% c("oui", "équivalent")) ) ) {
    result <- f("Les rapporteurs doivent obligatoirement posséder l'HDR ou l'équivalence")
  }
  return(result)
}
check_gender_bias <- function(data, f) {
  result <- TRUE
  genre_ratio <- length(which(data$civilité == "mme"))/nrow(data) %>% round(., 1)
  if( genre_ratio != 1/2 ) {
    result <- f("La composition du jury doit assurer une représentation équilibrée de femmes et d'hommes.\n", 
      "La composition que vous avez proposé sous-représente les ",
      ifelse(genre_ratio < 1/2, "femmes", "hommes"),"." )
  }
  return(result)
}

#' Définition de la sortie des fonctions en cas d'erreurs
#'
#' @param binary logical. Define if the functions should return TRUE/FALSE or a
#' message   
#' 
#' @return a list of function
#'
#' @export
alert_functions <- function(binary = FALSE, gender_biais = FALSE) {
  # Check validité du jury
  if(binary == FALSE){
    mystop <- stop
    mymessage <- message 
    mywarning <- warning
  } else {
    mystop <- function(x, ...) {
      return(FALSE)
    }
    mymessage <- function(x,...) {
      return(TRUE)
    } 
    if (gender_biais) {
      mywarning <- function(x,...) {
	return(FALSE)
      }
    } else {
      mywarning <- function(x,...) {
	return(TRUE)
      }
    }
  }

  return(list(mystop = mystop, mymessage = mymessage, mywarning = mywarning))
}
