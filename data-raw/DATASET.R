## code to prepare `DATASET` dataset goes here
library(tidyverse)
library(leaflet)
library(sf)

acpr <- read_sf("data-raw/DOH_ACPR_2018/DOH_ACPR_2018.shp") |>
  st_transform(7844) |>
  select(acpr_code = ACPR_Code, acpr_name = ACPR_Name, state = State_Terr)

usethis::use_data(acpr, overwrite = TRUE, compress = "xz")


# make phn dataset
phn <- read_sf("data-raw/phn.kml")
phn <- st_transform(phn, 7844)
phn <- phn[, colSums(is.na(phn)) < nrow(phn)] |>
  select(PHN_CODE = FIRST_PHN_, PHN_NAME, everything())

usethis::use_data(phn, overwrite = TRUE, compress = "xz")


# # map data from vic for 'LHN'
# read_sf("data-raw/vic-shapes/DHHSServiceAreas/DHHSServiceAreas.shp") |>
#   ggplot() +
#   geom_sf()
#
# vic_lga_mod <- read_sf("data-raw/vic-shapes/LGA_modified/LGA_modified.shp")
#
# x <- hpa.spatial::get_polygon("LGA2021") |>
#   filter(state_name_2021 == "Victoria") |>
#   mutate(LGA_NAME = toupper(lga_name_2021))
#
# x$LGA_NAME[!x$LGA_NAME %in% vic_lga_mod$LGA_NAME]
#
# read_sf("data-raw/vic-shapes/New File Geodatabase.gdb/")


# make lhn dataset
### create a complete dataset for LHNs (exception: Vic for now)
### NOTE: French Island is an SA2 but isn't contained within the LHN shapes data
###       Here, I assign it to the Bayside Peninsula LHN as it is closest.
french_island <- hpa.spatial::get_polygon("sa22021", crs = 7844) |>
  filter(sa2_name_2021 == "French Island") |>
  add_column(LHN_Name = "Bayside Peninsula", STATE_CODE = "2") |>
  select(LHN_Name, STATE_CODE)

vic_lhn <- read_sf("data-raw/lhn-vic/DHHS_Service_Areas.shp") |>
  select(LHN_Name = ServiceAre) |>
  add_column(STATE_CODE = "2") |>
  st_transform(7844) |>
  bind_rows(french_island) |>
  group_by(LHN_Name, STATE_CODE) |>
  summarize()

sa_lhn <- read_sf("data-raw/lhn-sa/LocalHealthNetworks_GDA2020.shp") |>
  select(LHN_Name = lhn) |>
  mutate(LHN_Name = str_trim(str_remove(LHN_Name, "LHN"))) |>
  add_column(STATE_CODE = "4")

wa_lhn <- read_sf("data-raw/lhn-wa/Health_Services_HEALTH_006.shp") |>
  mutate(STATE_CODE = "5") |>
  select(LHN_Name = service, STATE_CODE) |>
  st_transform(7844)

tas_lhn <- hpa.spatial::get_polygon("sa22021", crs = 7844) |>
  filter(state_code_2021 == 6) |>
  group_by(STATE_CODE = state_code_2021) |>
  summarize() |>
  ungroup() |>
  add_column(.before = 1, LHN_Name = "Tasmanian Health Service") |>
  st_transform(7844)

nt_lhn <- hpa.spatial::get_polygon("sa22021", crs = 7844) |>
  filter(state_code_2021 == 7) |>
  group_by(STATE_CODE = "7") |>
  summarize() |>
  ungroup() |>
  add_column(.before = 1, LHN_Name = "NT Regional Health Services (NTRHS)") |>
  st_transform(7844)

lhn <- read_sf("data-raw/LHN/Local_Hospital_Networks.shp") |>
  select(LHN_Name, STATE_CODE) |>
  filter(!STATE_CODE %in% c("4", "5", "6", "7")) |>
  st_transform(7844) |>
  bind_rows(vic_lhn) |>
  bind_rows(sa_lhn) |>
  bind_rows(wa_lhn) |>
  bind_rows(tas_lhn) |>
  bind_rows(nt_lhn) |>
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
mmm2019_labels <- c(
  "Metropolitan areas",
  "Regional centres",
  "Large rural towns",
  "Medium rural towns",
  "Small rural towns",
  "Remote communities",
  "Very remote communities"
)
mmm19 <- read_sf("data-raw/mmm-2019/MMM2019Final.shp") |>
  as.data.frame() |>
  select(SA1_MAIN16, MMM2019) |>
  mutate(MMM2019_label = factor(mmm2019_labels[MMM2019], levels = mmm2019_labels))

usethis::use_data(mmm19, overwrite = TRUE, compress = "xz")
