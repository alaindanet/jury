####################################
#  Methods to handle jury objects  #
####################################

#' @export

print.jury_list <- function(result, ...) {

    percent <- length(result$result)/result$nbcombination*100
    percent %<>% round(.)

    cat(percent, "% des combinaisons des membres de jury sont valides. \n")
}
