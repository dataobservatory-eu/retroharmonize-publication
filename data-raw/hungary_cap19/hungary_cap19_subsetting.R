library(dplyr)
library(rlang)
cap19_raw <- readxl::read_excel(
  file.path ( cap19_folder, 'data-raw', 'kantar', '19S8698_KULTURA_DP_SULY_20190625_LABEL.XLSX')
) %>%
  mutate ( uniqid = paste0("2019_CAP_HU_", ID)) %>%
  set_names (.,  gsub("wach_", "watch_", names(.))) %>%
  set_names (., gsub ("acquistion_", "acquisition_", names(.))) %>%
  set_names (., gsub ("unknonw", "unknown", names(.))) %>%
  set_names (., gsub ("phyiscal", "physical", names(.))) %>%
  set_names (., gsub ("casettte", "casette", names(.))) %>%
  set_names (., gsub ("Substitute_", "substitute_", names(.))) %>%
  set_names (., gsub ("acquisition_genre", "acquisition_music_genre", names(.)))

cap19_raw <- cap19_raw %>%
  rename ( watch_hours_av_youtube      = watch_av_hours_youtube,
           watch_hours_av_youtube_raw  = watch_av_hours_raw_youtube,
           watch_hours_av_physical_vhs = watch_hours_physical_vhs,
           watch_hours_av_physical_vhs_raw = watch_hours_raw_physical_vhs,
           watch_hours_av_physical_dvd = watch_hours_physical_dvd,
           watch_hours_av_physical_dvd_raw = watch_hours_raw_physical_dvd,
           listen_hours_raw_audiobook = listen_hours_audiobook_raw) %>%
  mutate ( age_exact = 2019 - year_birth) %>%
  mutate ( age_education = case_when (
    grepl("befejeztlen", education_highest) ~ NA_real_,
    TRUE ~ age_education_raw
  ))

hungary_cap19_subset <- cap19_raw %>%
  select ( 
    any_of(c("visit_library", "visit_cinema", "visit_concert", "visit_museum",
             "visit_historical_site", "artistic_activity_sung", "artistic_activity_played_music")),
    .data$subjective_urbanization,
    .data$life_satisfaction, .data$gender, .data$difficulty_bills, .data$megye,
    starts_with("marital_"),
    contains("occupation"),
    starts_with("age"),
    starts_with ("big_five"),
    contains('weight')
  ) %>%
  select (-any_of(contains("_category")),
          -any_of(contains("_type")),
          -any_of(contains("background"))) 

write.csv(hungary_cap19_subset, file.path("data-raw", "hungary_cap19_subset.csv"), row.names = F)
