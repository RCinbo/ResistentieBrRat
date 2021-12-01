dependents <- c("Resistent", "MutatieM1", "MutatieM2", "MutatieM3")
for (dependent in dependents) {
  output <- sprintf(
    "output/basemodel_%s.rds", dependent
  )
  if (file.exists(output)) {
    next
  }
  message("base: ", dependent, " ")
  bm <- base_model(
    first_order = TRUE, center_year = 2013, dependent = dependent
  )
  saveRDS(bm, output)
}
