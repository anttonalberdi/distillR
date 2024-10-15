test_that("compute_shortest_paths with the first MAG", {
  actual <-
    dram |>
    import_dram() |>
    dplyr::filter(mag_id == "GPB:bin_000004") |>
    dplyr::pull(annotation_id) |>
    compute_shortest_paths()

  # write_rds(actual, test_path("fixtures", "compute_shortest_paths.rds"), compress = "xz", version = 2, compression = 9)  # nolint

  expected <-
    test_path("fixtures", "compute_shortest_paths.rds") |>
    readr::read_rds()

  expect_equal(actual, expected)
})


test_that("compute_shortest_paths computes as complete an entire element", {
  # The Pathwar for B010101
  # K00764
  # (K01945,K11787,K11788,K13713)
  # (K00601,K11175,K08289,K11787,K01492)
  # (K01952,(K23269+K23264+K23265),(K23270+K23265))
  # (K01933,K11787,(K11788 (K01587,K11808,(K01589 K01588))))
  # (K01923,K01587,K13713)
  # K01756
  # (K00602,(K01492,(K06863 K11176)))

  annotations <- c(
    "K00601", "K00602", "K00764", "K01492", "K01587", "K01588", "K01589",
    "K01756", "K01923", "K01933", "K01945", "K01952", "K06863", "K08289",
    "K11176", "K11787", "K11788", "K11808", "K13713", "K23264", "K23265",
    "K23269", "K23270"
  )

  observed_cost <-
    annotations |>
    compute_shortest_paths() |>
    dplyr::filter(pathway_id == "B010101") |>
    dplyr::pull(cost)

  expect_equal(observed_cost, 0)
})


test_that("compute_shortest_paths for a straight line", {
  # The Pathwar for B010101
  # K00764
  # (K01945,K11787,K11788,K13713)
  # (K00601,K11175,K08289,K11787,K01492)
  # (K01952,(K23269+K23264+K23265),(K23270+K23265))
  # (K01933,K11787,(K11788 (K01587,K11808,(K01589 K01588))))
  # (K01923,K01587,K13713)
  # K01756
  # (K00602,(K01492,(K06863 K11176)))

  annotations <- c(
    "K00764", "K01945", "K00601", "K01952", "K01933", "K01923", "K01756",
    "K00602"
  )

  observed_cost <-
    annotations |>
    compute_shortest_paths() |>
    dplyr::filter(pathway_id == "B010101") |>
    dplyr::pull(cost)

  expect_equal(observed_cost, 0)
})
