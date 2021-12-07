library(readxl)
library(sf)
Hokken <- read_xlsx(find_root_file("data",
                                   "Kaartjes",
                                   "Hokken",
                                   "RowNBtoHokNb.xlsx",
                                   criterion =
                                     has_file("ResistentieBrRat.Rproj"))) %>%
  as.data.frame()
a <- which(Hokken$Bekken == "BE")
BESF <- read_sf(dsn = find_root_file("data", "Kaartjes", "Hokken",
                                     criterion =
                                       has_file("ResistentieBrRat.Rproj")),
                layer = "Beneden_Schelde_fishnet_clip") %>%
  mutate(BekkenNaam = "Beneden Schelde",
         Bekkennummer = ifelse(is.na(Hokken[a,'bekkennummer']),
                               NA,
                               str_c(Hokken[a,'Bekken'][1],
                                     Hokken[a,'bekkennummer'], sep = "")),
         Bekkenkleur = ifelse(is.na(Hokken[a,'bekkennummer']),
                              str_c(Hokken[a,'Bekken'][1],
                                    Hokken[a,"closestneighbor"]),
                              str_c(Hokken[a,'Bekken'][1],
                                    Hokken[a,'bekkennummer'], sep = "")))
# #Check nummers van de hokken
# BESF %>% mutate(nb = 1:nrow(BESF))%>%
#     st_as_sf %>%
#     st_cast("POLYGON", group_or_split = TRUE, warn = FALSE) %>%
#     ggplot() +
#     geom_sf(aes(fill = nb)) +
#     geom_sf_label(aes(label = nb))
# BESF %>%
#     st_as_sf %>%
#     st_cast("POLYGON", group_or_split = TRUE, warn = FALSE) %>%
#     ggplot() + geom_sf() +
#     geom_sf_label(aes(label = Bekkennummer))

a <- which(Hokken$Bekken == "BO")
BOSF <- read_sf(dsn = find_root_file("data", "Kaartjes", "Hokken",
                                     criterion =
                                       has_file("ResistentieBrRat.Rproj")),
                layer = "Boven_Schelde_fishnet_clip") %>%
  mutate(BekkenNaam = "Boven Schelde",
         Bekkennummer = ifelse(is.na(Hokken[a,'bekkennummer']),
                               NA,
                               str_c(Hokken[a,'Bekken'][1],
                                     Hokken[a,'bekkennummer'], sep = "")),
         Bekkenkleur = ifelse(is.na(Hokken[a,'bekkennummer']),
                              str_c(Hokken[a,'Bekken'][1],
                                    Hokken[a,"closestneighbor"]),
                              str_c(Hokken[a,'Bekken'][1],
                                    Hokken[a,'bekkennummer'], sep = "")))
a <- which(Hokken$Bekken == "BP")
BPSF <- read_sf(dsn = find_root_file("data", "Kaartjes", "Hokken",
                                     criterion =
                                       has_file("ResistentieBrRat.Rproj")),
                layer = "Brugse_polders_fishnet_clip") %>%
  mutate(BekkenNaam = "Brugse polders",
         Bekkennummer = ifelse(is.na(Hokken[a,'bekkennummer']),
                               NA,
                               str_c(Hokken[a,'Bekken'][1],
                                     Hokken[a,'bekkennummer'], sep = "")),
         Bekkenkleur = ifelse(is.na(Hokken[a,'bekkennummer']),
                              str_c(Hokken[a,'Bekken'][1],
                                    Hokken[a,"closestneighbor"]),
                              str_c(Hokken[a,'Bekken'][1],
                                    Hokken[a,'bekkennummer'], sep = "")))
a <- which(Hokken$Bekken == "DD")
DDSF <- read_sf(dsn = find_root_file("data", "Kaartjes", "Hokken",
                                     criterion =
                                       has_file("ResistentieBrRat.Rproj")),
                layer = "Dender_fishnet_clip") %>%
  mutate(BekkenNaam = "Dender",
         Bekkennummer = ifelse(is.na(Hokken[a,'bekkennummer']),
                               NA,
                               str_c(Hokken[a,'Bekken'][1],
                                     Hokken[a,'bekkennummer'], sep = "")),
         Bekkenkleur = ifelse(is.na(Hokken[a,'bekkennummer']),
                              str_c(Hokken[a,'Bekken'][1],
                                    Hokken[a,"closestneighbor"]),
                              str_c(Hokken[a,'Bekken'][1],
                                    Hokken[a,'bekkennummer'], sep = "")))
a <- which(Hokken$Bekken == "DL")
DLSF <- read_sf(dsn = find_root_file("data", "Kaartjes", "Hokken",
                                     criterion =
                                       has_file("ResistentieBrRat.Rproj")),
                layer = "Dijle_fishnet_clip") %>%
  mutate(BekkenNaam = "Dijle",
         Bekkennummer = ifelse(is.na(Hokken[a,'bekkennummer']),
                               NA,
                               str_c(Hokken[a,'Bekken'][1],
                                     Hokken[a,'bekkennummer'], sep = "")),
         Bekkenkleur = ifelse(is.na(Hokken[a,'bekkennummer']),
                              str_c(Hokken[a,'Bekken'][1],
                                    Hokken[a,"closestneighbor"]),
                              str_c(Hokken[a,'Bekken'][1],
                                    Hokken[a,'bekkennummer'], sep = "")))
a <- which(Hokken$Bekken == "DM")
DMSF <- read_sf(dsn = find_root_file("data", "Kaartjes", "Hokken",
                                     criterion =
                                       has_file("ResistentieBrRat.Rproj")),
                layer = "Demer_fishnet_clip") %>%
  mutate(BekkenNaam = "Demer",
         Bekkennummer = ifelse(is.na(Hokken[a,'bekkennummer']),
                               NA,
                               str_c(Hokken[a,'Bekken'][1],
                                     Hokken[a,'bekkennummer'], sep = "")),
         Bekkenkleur = ifelse(is.na(Hokken[a,'bekkennummer']),
                              str_c(Hokken[a,'Bekken'][1],
                                    Hokken[a,"closestneighbor"]),
                              str_c(Hokken[a,'Bekken'][1],
                                    Hokken[a,'bekkennummer'], sep = "")))
a <- which(Hokken$Bekken == "GK")
GKSF <- read_sf(dsn = find_root_file("data", "Kaartjes", "Hokken",
                                     criterion =
                                       has_file("ResistentieBrRat.Rproj")),
                layer = "Gentse_kanalen_fishnet_clip") %>%
  mutate(BekkenNaam = "Gense kanalen",
         Bekkennummer = ifelse(is.na(Hokken[a,'bekkennummer']),
                               NA,
                               str_c(Hokken[a,'Bekken'][1],
                                     Hokken[a,'bekkennummer'], sep = "")),
         Bekkenkleur = ifelse(is.na(Hokken[a,'bekkennummer']),
                              str_c(Hokken[a,'Bekken'][1],
                                    Hokken[a,"closestneighbor"]),
                              str_c(Hokken[a,'Bekken'][1],
                                    Hokken[a,'bekkennummer'], sep = "")))
a <- which(Hokken$Bekken == "IZ")
IZSF <- read_sf(dsn = find_root_file("data", "Kaartjes", "Hokken",
                                     criterion =
                                       has_file("ResistentieBrRat.Rproj")),
                layer = "Ijzer_fishnet_clip") %>%
  mutate(BekkenNaam = "Ijzer",
         Bekkennummer = ifelse(is.na(Hokken[a,'bekkennummer']),
                               NA,
                               str_c(Hokken[a,'Bekken'][1],
                                     Hokken[a,'bekkennummer'], sep = "")),
         Bekkenkleur = ifelse(is.na(Hokken[a,'bekkennummer']),
                              str_c(Hokken[a,'Bekken'][1],
                                    Hokken[a,"closestneighbor"]),
                              str_c(Hokken[a,'Bekken'][1],
                                    Hokken[a,'bekkennummer'], sep = "")))
a <- which(Hokken$Bekken == "LE")
LESF <- read_sf(dsn = find_root_file("data", "Kaartjes", "Hokken",
                                     criterion =
                                       has_file("ResistentieBrRat.Rproj")),
                layer = "Leie_fishnet_clip") %>%
  mutate(BekkenNaam = "Leie",
         Bekkennummer = ifelse(is.na(Hokken[a,'bekkennummer']),
                               NA,
                               str_c(Hokken[a,'Bekken'][1],
                                     Hokken[a,'bekkennummer'], sep = "")),
         Bekkenkleur = ifelse(is.na(Hokken[a,'bekkennummer']),
                              str_c(Hokken[a,'Bekken'][1],
                                    Hokken[a,"closestneighbor"]),
                              str_c(Hokken[a,'Bekken'][1],
                                    Hokken[a,'bekkennummer'], sep = "")))
a <- which(Hokken$Bekken == "MA")
MASF <- read_sf(dsn = find_root_file("data", "Kaartjes", "Hokken",
                                     criterion =
                                       has_file("ResistentieBrRat.Rproj")),
                layer = "Maas2_fishnet_clip") %>%
  mutate(BekkenNaam = "Maas Antwerpen",
         Bekkennummer = ifelse(is.na(Hokken[a,'bekkennummer']),
                               NA,
                               str_c(Hokken[a,'Bekken'][1],
                                     Hokken[a,'bekkennummer'], sep = "")),
         Bekkenkleur = ifelse(is.na(Hokken[a,'bekkennummer']),
                              str_c(Hokken[a,'Bekken'][1],
                                    Hokken[a,"closestneighbor"]),
                              str_c(Hokken[a,'Bekken'][1],
                                    Hokken[a,'bekkennummer'], sep = "")))
a <- which(Hokken$Bekken == "ML")
MLSF <- read_sf(dsn = find_root_file("data", "Kaartjes", "Hokken",
                                     criterion =
                                       has_file("ResistentieBrRat.Rproj")),
                layer = "Maas1_fishnet_clip") %>%
  mutate(BekkenNaam = "Maas Limburg",
         Bekkennummer = ifelse(is.na(Hokken[a,'bekkennummer']),
                               NA,
                               str_c(Hokken[a,'Bekken'][1],
                                     Hokken[a,'bekkennummer'], sep = "")),
         Bekkenkleur = ifelse(is.na(Hokken[a,'bekkennummer']),
                              str_c(Hokken[a,'Bekken'][1],
                                    Hokken[a,"closestneighbor"]),
                              str_c(Hokken[a,'Bekken'][1],
                                    Hokken[a,'bekkennummer'], sep = "")))
a <- which(Hokken$Bekken == "NE")
NESF <- read_sf(dsn = find_root_file("data", "Kaartjes", "Hokken",
                                     criterion =
                                       has_file("ResistentieBrRat.Rproj")),
                layer = "Nete_fishnet_clip") %>%
  mutate(BekkenNaam = "Nete",
         Bekkennummer = ifelse(is.na(Hokken[a,'bekkennummer']),
                               NA,
                               str_c(Hokken[a,'Bekken'][1],
                                     Hokken[a,'bekkennummer'], sep = "")),
         Bekkenkleur = ifelse(is.na(Hokken[a,'bekkennummer']),
                              str_c(Hokken[a,'Bekken'][1],
                                    Hokken[a,"closestneighbor"]),
                              str_c(Hokken[a,'Bekken'][1],
                                    Hokken[a,'bekkennummer'], sep = "")))

Hokken <- rbind(BESF, BOSF, BPSF, DDSF, DLSF, DMSF, GKSF, IZSF, LESF, MASF,
                MLSF, NESF) %>%
  mutate(BekkenNaam = as.factor(BekkenNaam),
         Bekkennummer = as.factor(Bekkennummer),
         Bekkenkleur = as.factor(Bekkenkleur))
write_sf(obj = Hokken,
         dsn = find_root_file("data", "Kaartjes", "Hokken", "Hokken.shp",
                              criterion =
                                has_file("ResistentieBrRat.Rproj")),
         delete_dsn = TRUE)


