library(tibble)
library(dplyr)

context("Tester la composition du jury")

# Jury data
civilité <- c("Mme", "Mme", "M", "M", "Mme", "M")
rang <- c("A", "A", "B", "B", "A", "B")
hdr <- c("Oui", "Oui", "Équivalent", "Non", "Oui", "Non")
local <- c("Extérieur", "Local", "Extérieur", "Extérieur", "Local", "Local")
role <- c("Rapporteur", "Examinateur", "Rapporteur", "Examinateur", "Directeur", "Encadrant")

## Good jury
jury_example <- tibble::as_tibble(cbind(civilité, rang, hdr, local, role))

## Bad input
jury_bad_input <- dplyr::rename(jury_example, civilite = civilité)
jury_bad_input2 <- dplyr::mutate(jury_example, nom = NA) %>%
    dplyr::rename(civilite = civilité)
jury_bad_civilité <- dplyr::mutate(jury_example, civilité = c("Mr", "Mme", "M", "NA", "Mrs", "M"))
jury_bad_rang <- dplyr::mutate(jury_example, rang = c("C", "NA", "B", "A", "B", "D"))
jury_bad_hdr <- dplyr::mutate(jury_example, 
    hdr = c("Oui", "NA", "Equivalent", "Non", " ", "Non"))
jury_bad_local <- dplyr::mutate(jury_example, 
    local = c("montpellier", "Local", "Extérieur", "paris", "Local", "Local"))
jury_bad_role <- dplyr::mutate(jury_example, 
    role = c("Rapporteur", "Examinateur", "co-directeur", "Examinateur", "Rapporteur", "Encadrant"))

test_that("Data format check", {
    expect_error(jury_check()) #, "Veuillez fournir une composition de jury!"
    expect_error(jury_check(jury_bad_input), 
	"data doit au moins contenir les variables suivantes: civilité, rang, hdr, local, role")
    expect_error(jury_check(jury_bad_input2), 
	"data doit au moins contenir les variables suivantes: civilité, rang, hdr, local, role")
    expect_error(jury_check(jury_bad_civilité),
	"La civilité fournie ne peut être que Mme ou M")
    expect_error(jury_check(jury_bad_rang),
	"Le rang fourni ne peut être que A ou B")
    expect_error(jury_check(jury_bad_hdr),
	"Le hdr fourni ne peut être que Oui, Non ou Équivalent")
    expect_error(jury_check(jury_bad_local),
	"La localité fournie ne peut être que Local ou Extérieur")
    expect_error(jury_check(jury_bad_role),
	"Le role fourni ne peut être que Rapporteur, Directeur, Encadrant ou Examinateur")
})

## Bad jury
jury_bad_nb <- dplyr::slice(jury_example, 1:4)
jury_bad_nb_sup <- rbind(jury_example, jury_example[2,])
jury_bad_nb_rank <- dplyr::mutate(jury_example, rang = c("A", "A", "B", "B", "B", "B"))
jury_bad_nb_loc <- dplyr::mutate(jury_example, 
    local = c("Local", "Local", "Extérieur", "Extérieur", "Local", "Local"))
jury_bad_nb_rap <- dplyr::mutate(jury_example, 
    role = c("Rapporteur", "Examinateur", "Examinateur", "Examinateur", "Directeur", "Encadrant"))
jury_bad_nb_exa <- dplyr::mutate(jury_example, 
    role = c("Rapporteur", "Examinateur", "Rapporteur", "Rapporteur", "Directeur", "Encadrant"))
jury_bad_hdr_rap <- dplyr::mutate(jury_example, 
    hdr = c("Non", "Non", "Équivalent", "Non", "Oui", "Non"))
jury_bad_hdr_dir <- dplyr::mutate(jury_example, 
    hdr = c("Oui", "Non", "Équivalent", "Non", "Non", "Non"))
jury_bad_dir <- dplyr::mutate(jury_example, 
    role = c("Rapporteur", "Examinateur", "Rapporteur", "Examinateur", "Examinateur", "Encadrant"))
jury_bad_parite <- dplyr::mutate(jury_example, 
    civilité = c("Mme", "M", "M", "M", "Mme", "M"))

test_that("Jury composition check works", {
  expect_error(jury_check(jury_bad_nb),
    "Le jury doit être composé de cinq membres au minimum")
  expect_error(jury_check(jury_bad_nb_sup),
    "Le jury doit être composé de cinq membres au minimum")
  expect_error(jury_check(jury_bad_nb_rank),
    "La moitié des membres jury au minimum doit être de rang A")
  expect_error(jury_check(jury_bad_nb_loc),
    "La moitié des membres jury au minimum doit être rattaché à une école doctorale extérieure")
  expect_error(jury_check(jury_bad_nb_rap),
    "Le jury doit être composé de deux rapporteurs")
  expect_error(jury_check(jury_bad_nb_exa),
    "Le jury doit être composé de deux examinateurs")
  expect_error(jury_check(jury_bad_hdr_rap), 
    "Les rapporteurs doivent obligatoirement posséder l'HDR ou l'équivalence")
  expect_error(jury_check(jury_bad_hdr_dir),
    "Les directeurs doivent obligatoirement posséder l'HDR")
  expect_error(jury_check(jury_bad_dir),
    "Le jury doit avoir un directeur de thèse.")
  expect_warning(jury_check(jury_bad_parite),
    "La composition du jury doit assurer une représentation équilibrée de femmes et d'hommes.")
  expect_message(jury_check(jury_example),
    "La composition de jury de thèse que vous avez proposé semble valide.")
})

test_that("Jury composition check works when binary", {
  expect_false(jury_check(jury_bad_nb, binary = TRUE))
  expect_false(jury_check(jury_bad_nb_sup, binary = TRUE))
  expect_false(jury_check(jury_bad_nb_rank, binary = TRUE))
  expect_false(jury_check(jury_bad_nb_loc, binary = TRUE))
  expect_false(jury_check(jury_bad_nb_rap, binary = TRUE))
  expect_false(jury_check(jury_bad_nb_exa, binary = TRUE))
  expect_false(jury_check(jury_bad_hdr_rap, binary = TRUE))
  expect_false(jury_check(jury_bad_hdr_dir, binary = TRUE))
  expect_false(jury_check(jury_bad_dir, binary = TRUE))
  expect_false(jury_check(jury_bad_parite, binary = TRUE, gender_biais = TRUE))
  expect_true(jury_check(jury_example, binary = TRUE))
})

test_that("People suggestion check", {

    data(people_suggestion)
    valid_jury <- jury_check_all(people_suggestion)

    expect_error(jury_check_all(people_suggestion, n = 3),
	"n ne peut que prendre les valeurs 5 et 6.")
    expect_error(jury_check_all(people_suggestion, n = 7),
	"n ne peut que prendre les valeurs 5 et 6.")
    expect_output(str(valid_jury),
	"List of 3")
    expect_equal(length(valid_jury$result),
	6)
})
