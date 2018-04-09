context("Test that the condition functions work")

require(magrittr) #needed to use pipe

## TODO: Rename context
## TODO: Add more tests

# Data
data(jury_example)

jury_example %<>% jury_validity()
jury_bad_dir <- dplyr::mutate(jury_example, 
    role = c("rapporteur", "examinateur", "examinateur", "rapporteur", "examinateur", "encadrant"))

test_that("la vérification des directeurs de thèse fonctionne", {
  expect_error(check_supervisor(jury_bad_dir))
})

test_that("la fonction alerte retourne une liste de fonction", {
  expect_output(str(alert_functions(TRUE)), "List of 3")
})

