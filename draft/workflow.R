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

library(tibble)
tibble()
tibble(x = 1:5, y = 1, z = x ^ 2 + y)
packageVersion("tibble")
devtools::install_version("tibble", version = "1.3.3", repos = "http://cran.us.r-project.org")
version


##########################
#  Build a new vignette  #
##########################

devtools::use_vignette("set_jury_composition")
