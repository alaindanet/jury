######################
#  Workflow to test  #
######################

library(devtools)
devtools::document()
devtools::test()

str(people_suggestion)
valid_jury <- jury_check_all(data = people_suggestion)
str(valid_jury)
names(valid_jury)

devtools::load_all()
print(valid_jury, n = 3)

valid_jury$result[[1]]

test <-  as.data.frame(people_suggestion)
test2 <- jury_check_all(test)
print(test2)

df <- valid_jury$result %>% purrr::reduce(bind_rows)

result <- with(test <- alert_functions(TRUE), {
  res <- c(
    check_nb_exam(jury_bad_nb, mystop),
    check_nb_reviewer(jury_bad_nb, mystop),
    check_nb_row(jury_bad_nb, mystop)
    )

res
})
result


jury_check(jury_bad_nb, binary = TRUE)
test <- c(T,T,T,T)
any(test == FALSE)

##########################
#  Build a new vignette  #
##########################

devtools::use_vignette("set_jury_composition")

