#' Fit a base model
#' @param first_order Use first (`TRUE`) or second (`FALSE`) order random walk
#' for the year component.
#' Defaults to `TRUE`.
#' @param center_year The year to center to.
#' Defaults to `2001`.
#' @export
#' @importFrom assertthat assert_that is.flag noNA
#' @importFrom dplyr arrange bind_cols distinct inner_join mutate select %>%
#' @importFrom git2rdata read_vc
#' @importFrom rlang .data !!
#' @importFrom sf st_as_sf st_coordinates st_drop_geometry st_transform
#' @importFrom tidyr complete
base_model <- function(
  first_order = TRUE, center_year = 2013, dependent = "Resistent"
) {
  assert_that(is.flag(first_order), noNA(first_order))
  read_excel(path = find_root_file("data",
                                   "2013 - 2019 BMK.xlsx",
                                   criterion =
                                     has_file("ResistentieBrRat.Rproj"))) ->
    base_data

  base_data %>%
    rename(X = X_lambert,
           Y = Y_lambert,
           year = jaar,
           location = Bekkennummer
           ) %>%
    mutate(location = as.factor(location)) %>%
    mutate(
      iyear = .data$year - min(.data$year) + 1,
      iyear2 = .data$iyear,
      cyear = .data$year - center_year,
      X = .data$X / 1e3, Y = .data$Y / 1e3,
      secondary = NA_real_,
      Resistent = 1*(!(mutatie =='WW')),
      MutatieM1 = 1*(str_detect(mutatie,regex('M1'))),
      MutatieM2 = 1*(str_detect(mutatie,regex('M2'))),
      MutatieM3 = 1*(str_detect(mutatie,regex('M3')))
    ) %>%
    select(.data$year, .data$Bekken, .data$X, .data$Y, .data$mutatie,
           .data$location, Resistent = !!dependent,
           iyear, iyear2, cyear, X, Y, secondary) -> base_data
  base_data %>%
    distinct(.data$year) %>%
    arrange(.data$year) %>%
    mutate(
      iyear = .data$year - min(.data$year) + 1,
      iyear2 = .data$iyear,
      cyear = .data$year - center_year,
      intercept = 1,
      secondary = NA_real_
    ) -> trend_prediction
  base_data %>%
    mutate(
      iyear = .data$year - min(.data$year) + 1,
      iyear2 = .data$iyear,
      cyear = .data$year - center_year,
      secondary = NA
    ) %>%
    arrange(.data$location, .data$year) -> base_prediction
  results <- fit_model(
    first_order = first_order, base_data = base_data,
    trend_prediction = trend_prediction, base_prediction = base_prediction
  )
  return(
    c(
      dependent = dependent, results, type = "base"
    )
  )
}
