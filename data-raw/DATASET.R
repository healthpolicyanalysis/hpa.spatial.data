## code to prepare `DATASET` dataset goes here
library(tidyverse)
library(leaflet)
library(sf)

# make phn dataset
phn <- read_sf("data-raw/phn.kml")
phn <- st_transform(phn, 7844)
phn <- phn[, colSums(is.na(phn)) < nrow(phn)] |>
  select(PHN_CODE = FIRST_PHN_, PHN_NAME, everything())

usethis::use_data(phn, overwrite = TRUE, compress = "xz")

# make lhn dataset
### create a complete dataset for LHNs (exception: Vic for now)
vic_lhn <- read_sf("data-raw/lhn-vic/DHHS_Service_Areas.shp") |>
  select(LHN_Name = ServiceAre) |>
  add_column(STATE_CODE = "2") |>
  st_transform(7844)

sa_lhn <- read_sf("data-raw/lhn-sa/LocalHealthNetworks_GDA2020.shp") |>
  select(LHN_Name = lhn) |>
  mutate(LHN_Name = str_trim(str_remove(LHN_Name, "LHN"))) |>
  add_column(STATE_CODE = "4")

lhn <- read_sf("data-raw/LHN/Local_Hospital_Networks.shp") |>
  select(LHN_Name, STATE_CODE) |>
  filter(STATE_CODE != "4") |>
  st_transform(7844) |>
  bind_rows(vic_lhn) |>
  bind_rows(sa_lhn) |>
  mutate(
    state = toupper(strayr::clean_state(STATE_CODE)),
    state = factor(
      state,
      levels = data.frame(state = unique(state), STATE_CODE = unique(STATE_CODE)) |>
        arrange(STATE_CODE) |>
        pull(state)
    )
  ) |>
  select(LHN_Name, state, STATE_CODE) |>
  arrange(STATE_CODE, LHN_Name)

usethis::use_data(lhn, overwrite = TRUE, compress = "xz")


# create qld meshblocks dataset with column for population
# for testing make_correspondence_tbl()

mb21_poly <- read_sf("data-raw/mb/MB_2021_AUST_GDA2020.shp")

mb21_poly_A <- filter(mb21_poly, STE_CODE21 %in% c(1, 2))
usethis::use_data(mb21_poly_A, overwrite = TRUE, compress = "xz")

rm(mb21_poly_A)
gc()

mb21_poly_B <- filter(mb21_poly, !STE_CODE21 %in% c(1, 2))
usethis::use_data(mb21_poly_B, overwrite = TRUE, compress = "xz")
rm(mb21_poly_B)


aus_mb21_points <- st_point_on_surface(mb21_poly)
rm(mb21_poly)
gc()

aus_mb21_pops <- lapply(2:13, \(x) {
  readxl::read_xlsx("data-raw/mb/Mesh Block Counts, 2021.xlsx", skip = 6, sheet = x)
}) |>
  (\(x) do.call("rbind", x))()


mb21_pop <- left_join(
  aus_mb21_points,
  aus_mb21_pops,
  by = c("MB_CODE21" = "MB_CODE_2021")
)

usethis::use_data(mb21_pop, overwrite = TRUE, compress = "xz")


### create MMM 2019 dataset

mmm19 <- read_sf("data-raw/mmm-2019/MMM2019Final.shp") |>
  as.data.frame() |>
  select(SA1_MAIN16, MMM2019)

usethis::use_data(mmm19, overwrite = TRUE, compress = "xz")
