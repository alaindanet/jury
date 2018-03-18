######################
#  Workflow to test  #
######################

library(devtools)
devtools::document()
devtools::test()

str(people_suggestion)
valid_jury <- jury_check_all(data = people_suggestion)
str(valid_jury)
print(valid_jury, n = 3)

df <- valid_jury$result %>% purrr::reduce(bind_rows)

m <- tibble()
tibble(x = 1:5, y = 1, z = x ^ 2 + y)
