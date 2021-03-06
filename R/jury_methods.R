####################################
#  Methods to handle jury objects  #
####################################

#' Summuary of valid jury combination
#'
#' @export
summary.jury_list <- function(result, ...) {

    percent <- length(result$result) / result$nbcombination * 100
    percent %<>% round(.)

    cat(percent, "% des combinaisons des membres de jury sont valides. \n")
}

#' Print Jury
#'
#' @export
print.jury_list <- function(jurylist, n = 3) {
    top <- jurylist$result[1:n]

    cat("Vos compositions de jury préférées sont: \n")
    for (i in seq(1, n)) {
	cat("Numéro", i, ": \n")
	cat("--------")
	cat("\n")
	print(as.data.frame(top[[1]]), na.print = "", quote = FALSE)
	cat("\n")
	cat("\n")

    }
}
