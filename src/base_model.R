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
                                   "data_2013_2024.xls",
                                   criterion =
                                     has_file("ResistentieBrRat.Rproj"))) %>% 
    subset(!is.na(mutatie)) -> base_data

  base_data %>%
    rename(X = X_lambert,
           Y = Y_lambert,
           location = Bekkennummer) %>%
    mutate(location = as.factor(location)) %>%
    mutate(
      year = ifelse(is.na(year(datum)),
                    jaar,
                    year(datum)
      ),
      iyear = .data$year - min(.data$year) + 1,
      iyear2 = .data$iyear,
      cyear = .data$year - center_year,
      X = .data$X / 1e3, Y = .data$Y / 1e3,
      secondary = NA_real_,
      Resistent = 1*(!(mutatie == 'WW')),
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
  unique_data <- expand.grid(year = unique(base_data$year), 
                             location = unique(base_data$location))
  unique_data$Bekken <- str_sub(unique_data$location, 1 , 2)
  Hokken <- read_sf(dsn = find_root_file("data", "Kaartjes", "Hokken",
                                         criterion = 
                                           has_file("ResistentieBrRat.Rproj"))
                    ,layer = "Hokken"
  ) %>%
    st_transform("EPSG:31370")
  Hokken <- Hokken %>% mutate(Bekken_klr = substr(Bkknklr, start = 1, 
                                                  stop = 2),
                              Bekken_nmm = substr(Bkknnmm, start = 1, 
                                                  stop = 2),
                              Number_klr = substr(Bkknklr, start = 3, 
                                                  stop = 4), 
                              Number_nmm = substr(Bkknnmm, start = 3, 
                                                  stop = 4)) %>% 
    rowwise() %>% 
    mutate(Number_length_klr = nchar(Number_klr), 
           Number_length_nmm = nchar(Number_nmm))
  
  Hokken <- Hokken %>% mutate(Bkknklr  = 
                                ifelse(Number_length_klr == 2, 
                                       Bkknklr, paste0(Bekken_klr, 
                                                       paste0("0", Number_klr))), 
                              Bkknnmm  = ifelse(Number_length_nmm == 2, 
                                                Bkknnmm, paste0(Bekken_nmm, 
                                                                paste0("0", Number_nmm)))
  ) %>% 
    select(c(1:6))
  
  Hokken <- cbind(st_coordinates(st_centroid(Hokken))/1e3, st_drop_geometry(Hokken))
  Hokken <- Hokken %>% select(-c(Shap_Ar, Shp_Lng))
  missing_hokken <- na.omit(setdiff(Hokken$Bkknnmm, unique_data$location))
  missing_hokken <- data.frame(year = rep(unique(unique_data$year), 2), 
                               location = rep(missing_hokken, 
                                              each = length(unique(unique_data$year))), 
                               Bekken = rep(str_sub(missing_hokken, 1, 2), 
                                            each = length(unique(unique_data$year)))
  )
  unique_data <- rbind(unique_data, missing_hokken)
  base_data_expand <- inner_join(unique_data, Hokken, join_by(location == Bkknnmm ))
  # missing_hokken <- anti_join(base_data, base_data_expand, by = "location" ) %>% 
  #   distinct(location, .keep_all = TRUE)
  # missing_hokken_expnd <- expand.grid(year = unique(base_data$year), 
  #                                     location = unique(missing_hokken$location)) %>% 
  #                                       merge(missing_hokken %>% select(
  #                                         location, mutatie, X, Y, Bekken))
  # base_data_expand <- full_join(base_data_expand, missing_hokken_expnd)
  # 1) expand.grid voor blok en jaar 
 # 2) voor elke blok een coördinaten zijnde het midden van de blok
 # 3) sf_centroid voor elke blok uit de polynoom (st_centroid)
 # 4) die twee linken aan elkaar 
 # 5) voor elk centrum voor elk blok de predicties gaan doen. 
 # unique combinatie van jaar en data en dan complete of expand.grid van jaar en locatie 
 # years <- base_data %>% distinct(year) %>% pull(year)
 # base_data_expand <- unique_data %>% crossing(year = years)
 # base_data_expand <- unique_data
 base_data_expand %>% 
    mutate(
      iyear = .data$year - min(.data$year) + 1,
      iyear2 = .data$iyear,
      cyear = .data$year - center_year,
      secondary = NA
    ) %>%
    arrange(.data$location, .data$year) -> all_prediction
  results <- fit_model(
    first_order = first_order, base_data = base_data,
    trend_prediction = trend_prediction, base_prediction = base_prediction, 
    all_prediction = all_prediction
  )
  return(
    c(
      dependent = dependent, results, type = "base"
    )
  )
}
