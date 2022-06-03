## (c) Antal Dániel, 2019.
## Az alábbi programkód felhasználásához a szerző célhoz kötötten járul hozzá.
## A programkódot kizárólag az Artisjus és a GVH megbízottai használhatják
## és kizárólag a CAP 2019 survey adatainak ellenőrzésére vagy utóellenőrzésére.
## A szerző, Antal Dániel nem járul hozzá a forráskód más programokba való
## beépítéséhez és újrafelhasználásához.

library(tidyverse)
source ( file.path("R", "folders.R"))
source ( file.path("R", "process_survey.R"))

cap_folder <- file.path ("C:", "Users", "Daniel Antal",
                              "OneDrive - Visegrad Investments",
                              "2019 Projektek",
                              "HU_CAP19")

uhd_folder <- file.path ("C:", "Users", "Daniel Antal",
                               "OneDrive - Visegrad Investments",
                               "2019 Projektek",
                               "UHD19")
dir (file.path(cap_folder, 'data-raw'))

cap19_summaries <- readxl::read_excel(
  file.path ( cap_folder,  'data-raw', 'kantar', '19S8698_KULTURA_DP_SULY_20190625_LABEL.XLSX')
  )

summaries <- cap19_summaries %>%
  rename ( uniqid = ID ) %>%
  select ( uniqid, proj_weight_10,
           starts_with ( 'reality_check'))

names ( summaries )

reality_check_download_film <- cap19_summaries %>%
  rename ( uniqid = ID ) %>%
  select ( uniqid, proj_weight_10, starts_with ( 'reality_check')) %>%
  select ( uniqid, proj_weight_10, contains ( "music"))

source(file.path("R", "process_survey.R")) #low-level conversion functions

#download_tvprg_lcomputerwork

cap19_raw <- readxl::read_excel(
  file.path ( 'data-raw', 'kantar', '19S8698_KULTURA_DP_SULY_20190625_LABEL.XLSX')
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

weights <-  cap19_raw %>% select (uniqid, contains ("weight"))
saveRDS ( weights, file.path(uhd_folder, "data", "2019", "weights_2019.rds"))


paid_rates  <-  cap19_raw %>% select ( uniqid, contains ( "paid"))  %>%
  select ( uniqid, contains("rate"),
           store_music_casette_paid ) %>%
  select ( -contains("house"), -contains("occupation"),
           -download_av_na_paid_rate ) %>%
  gather ( names, values, !!2:ncol(.)) %>%
  mutate ( values = ifelse (values > 100, NA, values )) %>%
  spread ( names, values )

cap19_raw %>% select ( uniqid, contains ( "store_music"))  %>%
  names ( )

saveRDS ( paid_rates,
          file = file.path('data', '2019', 'paid_rates.rds'))

cap19_raw %>%
  select ( contains('youtube')) %>%
  select ( contains ('watch')) %>%
  names ()

names (cap19_raw)[1:11]

keep_numeric <- function(x, valid_min = 0, valid_max = 9997 ) {
  x <- as.numeric(gsub("[^0-9,.]", "", x))
  ifelse ( x > valid_max,
           yes = NA,
           no = ifelse ( test =x<valid_min,
           yes = NA, no=  x))
}

vocabulary_youtube <- read.csv ( "data-raw/vocabulary_youtube.csv",
                                 stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  rbind ( tibble ( substitute = 'series_rent',
                   youtube = "sorozatot néz",
                   helyette = 'kölcsönözne mûsort')) %>%
  mutate ( helyette =  gsub("û", '\u0171', helyette ),
           helyette =  gsub("õ", '\u0151', helyette ),)

substitute <- cap19_raw %>%
  select ( uniqid, weight_10, starts_with ("substitute")) %>%
  mutate ( missings = rowSums(is.na(.))) %>%
  filter ( missings < max(missings) ) %>%
  select (-missings) %>%
  mutate_at ( vars (starts_with ("substitute")),
              ~.*weight_10 ) %>%
  summarize_at ( vars (starts_with ("substitute")),
                 ~mean(., na.rm=TRUE ) ) %>%
  gather ( substitute, values )  %>%
  mutate ( substitute = gsub("substitute_youtube_", "", substitute)) %>%
  left_join ( ., vocabulary_youtube, by = "substitute") %>%
  mutate ( helyette = fct_relevel(helyette, "mást csinálna")) %>%
  mutate ( youtube = paste("a YouTube-on ", youtube, "..."))

require(ggalluvial)

youtube_substitute_palette <- as.character(c("grey60", my_palette[['darkblue']], "grey50", my_palette[['yellow']],
                                             my_palette[['red']],my_palette[['darkblue']], my_palette[['lightblue']] ))

youtube_substitute <- ggplot(substitute,
                             aes(y = values, axis1 = youtube, axis2 = helyette)) +
  geom_alluvium(aes(fill = helyette), width = 1/12, reverse = TRUE) +
  geom_stratum(width = 1/12, fill = "white", color = "grey") +
  geom_label(stat = "stratum", label.strata = TRUE, check_overlap = TRUE, size = 2) +
  scale_x_continuous(breaks = 1:2, labels = c("mit csinálna\nhelyette...", "mostan\nYouTube használat")) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  scale_fill_manual ( values = youtube_substitute_palette ) +
  theme_void ()+
  theme ( legend.position = "bottom") +
  labs ( title = "Potenciális helyettesítés a YouTube néz\u0151i körében",
         subtitle = "Ha nem volnának elérhet\u0151k a filmek, sorozatok, tévéprogramok...",
         caption = "Forrás: Hungary CAP 2018.")

plot (youtube_substitute)

ggsave (filename = "plots/youtube_substitute_2019.jpg" ,
        plot=youtube_substitute,
        width = 24, height = 18, units = "cm")

youtube_substitute_vertical <- ggplot(substitute,
                                      aes(y = values, axis1 = helyette, axis2 = youtube)) +
  geom_alluvium(aes(fill = helyette), width = 1/12, reverse = TRUE) +
  geom_stratum(width = 1/12, fill = "white", color = "grey") +
  geom_text(stat = "stratum", label.strata = TRUE, check_overlap = TRUE, size = 2) +
  scale_x_continuous(breaks = 1:2, labels = c("mit csinálna\nhelyette...", "mostan\nYouTube használat")) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  scale_fill_manual ( values = youtube_substitute_palette ) +
  theme_void ()+
  theme ( legend.position = "bottom") + coord_flip() +
  labs ( title = "Potenciális helyettesítés a YouTube néz\u0151i körében",
         subtitle = "Ha nem volnának elérhet\u0151k a filmek, sorozatok, tévéprogramok...",
         caption = "Forrás: Hungary CAP 2018.")

plot (youtube_substitute_vertical)

ggsave (filename = "plots/youtube_substitute_vertical.jpg" ,
        plot=youtube_substitute_vertical,
        width = 24, height = 18, units = "cm")

save (youtube_substitute_vertical,
      youtube_substitute,
      youtube_substitute_palette,
      file = file.path ( uhd_folder, 'data', '2019', "youtube_substitute.rda"))


## Hangoskönyvek rátöltése ----------------------------------
## Nincsen mértékegység és egyszerűsítve kérdezzük.

audiobooks <- cap19_raw %>%
  select ( uniqid, contains (  "acquisition_audiobook" )) %>%
  gather ( vars, raw_value, !!2:ncol(.) ) %>%
  mutate ( raw_value = case_when (
    is.na(raw_value) ~ 0,
    raw_value > 9997 ~ 0,
    TRUE ~ as.numeric(raw_value)
  )) %>%
  spread ( vars,raw_value)

audiobook_table <- weights %>%
  left_join ( audiobooks, by = "uniqid" )

saveRDS( audiobook_table , file.path(cap_folder, "data", "audiobooks_19.rds") )
saveRDS( audiobook_table , file.path("data", "2019", "audiobooks_19.rds") )


## Audiovizuális tartalmak élvezete feldolgozás -----------------
## Figyelem, a youtube órák összesítve és bontva is szerepelnek!

cap19_raw %>%
  select ( uniqid,  starts_with ( "watch_"))  %>%
  names ( )


watch_review <- cap19_raw %>%
  select ( uniqid,  starts_with ( "watch_")) %>%
  select ( contains('review'))

watch_av_raw  <- cap19_raw %>%
  select ( uniqid,  starts_with ( "watch_")) %>%
  select ( -contains('film_source_foreign'),
           -contains('total_film'),
           -contains('review'),
           -contains("rate"))  %>%
  select ( uniqid, contains ( "hours")) %>%
  select ( uniqid, contains ( 'raw')) %>%
  gather(  media, raw_value, !!2:ncol(.)) %>%
  mutate ( media = gsub("watch_hours_", "", media )) %>%
  mutate ( media = gsub("raw_|_raw", "", media )) %>%
  mutate ( raw_value = case_when (
    is.na(raw_value) ~ 0,
    raw_value > 9997 ~ 0,
    TRUE ~ as.numeric(raw_value)
  ))

#watch av units not same as listen units

watch_av_units <- cap19_raw %>%
  select ( uniqid,  starts_with ( "watch_")) %>%
  select ( -contains('film_source_foreign'),
           -contains('total_film'),
           -contains('review'),
           -contains("rate"))  %>%
  select ( uniqid, contains ( "hours")) %>%
  select ( -contains ('raw')) %>%
  mutate_all ( ~gsub("NEM NÉZ", 0, .)) %>%
  mutate_all ( ~gsub("NEM HASZNÁL", 0, .)) %>%
  mutate_all ( ~gsub("havonta", 12, .)) %>%
  mutate_all ( ~gsub("naponta", 250, .)) %>%
  mutate_all ( ~gsub("hetente", 50, .)) %>%
  mutate_all ( ~gsub("havi", 12, .)) %>%
  mutate_all ( ~gsub("napi", 250, .)) %>%
  mutate_all ( ~gsub("heti", 50, .)) %>%
  mutate_all ( ~gsub("NT-NV", NA, .)) %>%
  gather ( media, watch_unit, !!2:ncol(.)) %>%
  mutate ( media = gsub("watch_hours_", "", media ))  %>%
  mutate ( watch_av_unit = ifelse(is.na(watch_unit),
                                  yes = as.numeric(0),
                                  no = as.numeric(as.character(watch_unit)) )) %>%
  select ( -watch_unit )



unique ( watch_av_raw$media)
unique ( watch_av_units$media)

watch_av_combined <- left_join (
  watch_av_raw, watch_av_units,  by = c("uniqid", "media") ) %>%
  mutate ( media = gsub("watch_hours_", "", media ) ) %>%
  mutate ( new_var = paste0("watch_", media)) %>%
  mutate ( new_var = gsub("_av", "", new_var )) %>%
  mutate ( new_var = case_when (
    new_var  == 'watch_tv' ~ 'watch_broadcast_tv',
    new_var  == 'watch_youtube' ~ 'watch_youtube_youtube', #youtube összegző
    TRUE ~ as.character(new_var)
  )) %>%
  mutate ( value = watch_av_unit*raw_value ) %>%
  mutate ( value = ifelse ( value > 9000,
                            0, value ))  #egy lehetetlen válasz van

watch_av <- watch_av_combined %>%
  select ( uniqid, new_var, value ) %>%
  spread ( new_var, value )

summary (watch_av)


watch_av_2019 <- left_join ( weights, watch_av , by = 'uniqid')

saveRDS( watch_av, file.path("data", "watch_av.rds") )
saveRDS( watch_av, file.path(uhd_folder, "data", "watch_av.rds") )
saveRDS( watch_av_2019, file.path(uhd_folder, "data", "2019", "watch_av_2019.rds") )

## hipotetikus válaszok feldolgozása

hypothetical_values <- cap19_raw %>%
  select ( uniqid, contains ( "hyp")) %>%
  select ( -contains ("approval")) %>%
  mutate (price_hypothetical_youtube_premium_range =
              keep_numeric(price_hypothetical_youtube_premium_range) ) %>%
  mutate_if (is.numeric, ~process_numeric (., digits = 4,
                                           min_value = 0,
                                           max_value = 9997 )) %>%
  mutate ( youtube_hypothetical_price =
             ifelse ( !is.na(hypothetical_eval_youtube_red_raw),
                      hypothetical_eval_youtube_red_raw,
                      price_hypothetical_youtube_premium_range)) %>%
  select ( -hypothetical_eval_youtube_red_raw,
           - price_hypothetical_youtube_premium_range)

summary ( hypothetical_values$youtube_hypothetical_price[ hypothetical_values$youtube_hypothetical_price >0] )


## látogatás változók feldoglozása

visits <- process_numeric_table (
  cap19_raw,
  name_starts = "visit_",
  name_contains = NA,
  not_contains = c("category", "type", "last"),
  digits = 3,
  min_value = 0, max_value = 150,
  id = "uniqid",
  create_logical_var = FALSE )

artistic_activities <- process_numeric_table (
  cap19_raw,
  name_starts = "artistic_activity_",
  name_contains = NA,
  not_contains = c("category", "type", "last"),
  digits = 3,
  min_value = 0, max_value = 250,
  id = "uniqid",
  create_logical_var = FALSE )
warning ( "something goes to na")

activities <- process_numeric_table (
  cap19_raw,
  name_starts = "activity_",
  name_contains = NA,
  not_contains = c("category", "type", "last"),
  digits = 3,
  min_value = 0, max_value = 150,
  id = "uniqid",
  create_logical_var = FALSE )

## pszichometria - demográfiai kísérleti változó feldoglozása

big_five <- process_numeric_table (
  cap19_raw,
  name_starts = "big_five_",
  name_contains = NA,
  not_contains = c("category", "type", "last"),
  digits = 2,
  min_value = 0, max_value = 10,
  id = "uniqid",
  create_logical_var = FALSE )

unique ( cap19_raw$self_placement_society)

self_placement_society <- process_numeric_table (
  cap19_raw,
  name_starts = "self_placement_society",
  name_contains = NA,
  not_contains = c("category", "type", "last"),
  digits = 2,
  min_value = 0, max_value = 10,
  id = "uniqid",
  create_logical_var = FALSE )


cap19_raw %>%
  select ( -starts_with ( 'mc_'), -starts_with('listen'),
           -starts_with ( 'watch'), -starts_with ("big_five"),
           -starts_with ( 'store_'), -contains ( 'durable'),
           -contains ("weight")) %>%
  names ( )

## bináris változók feldolgozása
## 1 = pozitív, igen, vagy említette
## 0 = negatív, nem, vagy nem említette

## három lehetőségből bináris dummy
hypothetical_interest <- cap19_raw %>%
  select ( uniqid, starts_with ("is")) %>%
  select ( uniqid, contains("interest")) %>%
  mutate ( is_music_streaming_interest  =
             case_when (
               (str_sub(tolower(is_music_streaming_interest), 1,2) == 'nt') ~ "Nem válaszolt",
               TRUE ~ as.character(is_music_streaming_interest)
             )) %>%
  mutate ( is_music_streaming_interest = as.factor(is_music_streaming_interest)) %>%
  select ( -is_music_streaming_interest )

## három lehetőségből bináris dummy
is_film_or_music <- cap19_raw %>%
  select ( uniqid, interest_music_or_film ) %>%
  mutate ( is_film_only  = case_when (
    grepl( "Érdekel a zene is és a film is", interest_music_or_film) ~ 0,
    grepl( "Érdekelnek a filmek", interest_music_or_film) ~ 1,
    grepl( "Érdekel a zene", interest_music_or_film) ~ 0,
    grepl( "Sem a zene", interest_music_or_film) ~ 0,
    TRUE ~ NA_real_
  )) %>%
  mutate ( is_music_only  = case_when (
    grepl( "Érdekel a zene is és a film is", interest_music_or_film) ~ 0,
    grepl( "Érdekelnek a filmek", interest_music_or_film) ~ 0,
    grepl( "Érdekel a zene", interest_music_or_film) ~ 1,
    grepl( "Sem a zene", interest_music_or_film) ~ 0,
    TRUE ~ NA_real_
  )) %>%
  mutate ( is_music_film  = case_when (
    grepl( "Érdekel a zene is és a film is", interest_music_or_film) ~ 1,
    grepl( "Érdekelnek a filmek", interest_music_or_film) ~ 0,
    grepl( "Érdekel a zene", interest_music_or_film) ~ 0,
    grepl( "Sem a zene", interest_music_or_film) ~ 0,
    TRUE ~ NA_real_
  )) %>%
  mutate ( interest_music_or_film  = as.factor(interest_music_or_film ) )

binary_vars <- cap19_raw %>%
  select ( uniqid, starts_with ("is")) %>%
  select ( -starts_with("interest_music_or_film"),
           -contains("interest"))  %>%
  gather ( var_name, value, !! 2:ncol(.) ) %>%
  mutate ( value  = case_when (
    is.na(value) ~ NA_real_,
    str_sub(tolower(value),1,2) == 'nt'     ~ NA_real_,
    str_sub(tolower(value),1,3) == 'nem'    ~ 0,
    str_sub(tolower(value),1,5) == 'nincs'  ~ 0,
    str_sub(tolower(value),1,4) == 'igen'   ~ 1,
    str_sub(tolower(value),1,3) == 'van'    ~ 1
  )) %>%
  spread ( var_name, value )  %>%
  left_join ( is_film_or_music, by = 'uniqid' ) %>%
  left_join ( hypothetical_interest, by = 'uniqid')

summary (binary_vars)

youtube_downloaders <- left_join (
  select(weights, uniqid, proj_weight_10),
  select(binary_vars, uniqid, is_use_youtube_downloader),
  by = 'uniqid') %>%
  mutate ( is_use_youtube_downloader = na_to_0(is_use_youtube_downloader))

sum ( youtube_downloaders$is_use_youtube_downloader)

number_youtube_downloaders  <- youtube_downloaders %>%
  transmute ( is_use_youtube_downloader*proj_weight_10) %>%
  unlist () %>% sum( as.numeric ())


mc_use_device  <- cap19_raw %>%
  select ( uniqid, starts_with ( 'mc_use_')) %>%
  select ( -contains ( 'stream')) %>%
  process_cap_mc ( dat = .,
                   mc_name = "mc_use_") %>%
  set_names ( ., gsub("mc_use_", "", names(.)))

mc_use_device_sum <- mc_use_device %>% gather ( names, values, !!2:ncol(.)) %>%
  left_join ( select ( weights, uniqid, proj_weight_10), by = 'uniqid') %>%
  mutate ( proj_values = values * proj_weight_10 ) %>%
  group_by ( names ) %>%
  summarize_if ( is.numeric, sum, na.rm=TRUE )

## Többes választás feldolgozása ---------------------------------
## 1 = az elemet kiválasztota, 0 = elvetette
## hiányzó = nem lett neki feltéve vagy megtagadta a választ

unique(cap19_raw$mc_use_music_stream_other_free_str)

mc_use_free_music_stream  <- cap19_raw %>%
  select ( uniqid, starts_with ( 'mc_use_music_stream')) %>%
  select ( uniqid, contains ( '_free')) %>%
  select ( -mc_use_music_stream_other_free_str ) %>%
  set_names ( ., gsub("_free", "_freeremove", names(.))) %>%
  set_names ( ., gsub("music_stream_", "music_stream_free_", names(.))) %>%
  set_names ( ., gsub("_freeremove", "", names(.))) %>%
  process_cap_mc ( dat = .,
               mc_name = "mc_use_music_stream_free") %>%
  rename  ( is_use_music_stream_free_none = mc_use_music_stream_free_none )

mc_use_free_music_stream
summary ( mc_use_free_music_stream )
unique (cap19_raw$mc_use_music_stream_other_premium_str)

mc_use_premium_music_stream  <- cap19_raw %>%
  select ( uniqid, starts_with ( 'mc_use_music_stream')) %>%
  select ( uniqid, contains ( '_premium')) %>%
  mutate ( mc_use_music_stream_deezer_premium =ifelse (
    mc_use_music_stream_other_premium_str == "Deezer",
                    "eml", "nem")) %>%
  select ( -mc_use_music_stream_other_premium_str ) %>%
  set_names ( ., gsub("_premium", "_premiumremove", names(.))) %>%
  set_names ( ., gsub("music_stream_", "music_stream_premium_", names(.))) %>%
  set_names ( ., gsub("_premiumremove", "", names(.)))

mc_use_premium_music_stream <- mc_use_premium_music_stream %>%
  process_cap_mc ( dat = .,
                   mc_name = "mc_use_music_stream_premium") %>%
  rename  ( is_use_music_stream_premium_none = mc_use_music_stream_premium_none ) %>%
    mutate ( mc_use_music_stream_deezer_premium = case_when (
      is.na(mc_use_music_stream_premium_deezer) & ! is.na(mc_use_music_stream_premium_na) ~ 0,
      TRUE ~ as.numeric(mc_use_music_stream_premium_deezer)
    ))

save ( mc_use_premium_music_stream, mc_use_free_music_stream,
       weights, file = file.path( 'data', '2019', 'use_music_stream.rda'))


multiple_choice_1 <- cap19_raw %>%
  select ( uniqid, starts_with ( "mc_income_royalty" ),
           starts_with ( "mc_income_creative" ),
           starts_with ( "mc_internet_use" )) %>%
  gather ( vars, values, !!2:ncol(.)) %>%
  mutate ( values = tolower(values))  %>%
  mutate ( values = case_when (
    is.na(values) ~ NA_real_,
    str_sub(values, 1,3) == "nem"  ~ 0,
    str_sub(values, 1,4) == 'igen' ~ 1,
    str_sub(values, 1,3) == 'eml'  ~ 1,
    TRUE ~ NA_real_
  ))


unique ( multiple_choice_1$values) #csak NA, 0,1 értékek szerepelnek

multiple_choice_2 <- cap19_raw %>%
  select (  uniqid, starts_with ( "mc_") ) %>%
  select ( -starts_with ( "mc_income_royalty" ),
           -starts_with ( "mc_internet_use" ),
           -starts_with ( "mc_income_creative" ),
           -ends_with ("_str")) %>%
  gather ( vars, values, !! 2:ncol(.) ) %>%
  mutate ( values  = case_when (
    is.na(values) ~ NA_real_,
    values == 'említette' ~ 1,
    values == 'nem említette' ~ 0
  ))

unique ( multiple_choice_2$values) #csak NA, 0,1 értékek szerepelnek

multiple_choice <- rbind ( multiple_choice_1,  multiple_choice_2 ) %>%
  spread ( vars, values )

#ellenőrzés
if(nrow (multiple_choice) != nrow ( cap19_raw)) warning ( "Az elemszám nem egyezik")

summary (multiple_choice)

## Beszerzések feldolgozása ---------------------------------
## 1 album = 10 zeneszám

acquisition_genre <- cap19_raw %>%
  select ( starts_with("acquisition_music") )

summary (acquisition_genre )

cap19_raw %>% select ( contains ( "reality_")) %>%
  mutate_if ( is.character, as.factor ) %>%
  names  ( )

acquisition_av_source <- cap19_raw %>%
  select ( uniqid,
           starts_with("acquisition_film_source_"),
           starts_with("acquisition_series_source_")) %>%
  gather ( var_name, value, !! 2:ncol(.) ) %>%
  mutate ( value = process_numeric(value, digits = 4,
                                   min_value = 0,
                                   max_value = 9997))

acquisition_av_source <- cap19_raw %>%
  select (uniqid,
  starts_with ("acquisition_audiobook") ) %>%
  gather ( var_name, value, !! 2:ncol(.) ) %>%
  mutate ( value = process_numeric(value, digits = 4,
                                   min_value = 0,
                                   max_value = 9997))


acquisition_raw_music <- cap19_raw %>%
  select ( uniqid, starts_with("acquisition_music_")) %>%
  select ( -contains("reality"), -contains("rate") )

fn_empty <- function ( ) {

    select ( uniqid, contains("raw"), contains("source")) %>%
    gather ( var_name, value, !! 2:ncol(.) ) %>%
    mutate ( value = process_numeric(value, digits = 4,
                                     min_value = 0,
                                     max_value = 9997))

  acquisition_film <- cap19_raw %>%
    select ( uniqid, starts_with("acquistion_")) %>% names ( )


  cap19_raw %>% select ( -contains("reality"), -contains("genre"),
           -contains("rate"), -contains ("music") ) %>%
    select ( uniqid, contains("raw"), contains("source")) %>%
    gather ( var_name, value, !! 2:ncol(.) ) %>%
    mutate ( value = process_numeric(value, digits = 4,
                                     min_value = 0,
                                     max_value = 9997))

}


substitutes <- cap19_raw %>%
  select ( uniqid, contains("ubstit") )


acquisition_units <- cap19_raw %>%
  select ( uniqid, starts_with("acq") ) %>%
  select ( -contains("reality"), -contains("rate"), -contains("raw")) %>%
  select ( uniqid, contains("genre_"), contains ( "music_")) %>%
  gather ( var_name, value, !! 2:ncol(.) ) %>%
  filter ( nchar(value) > 6 ) %>%
  mutate ( value  = case_when (
    is.na(value) ~ NA_real_,
    value == 'zeneszámok mennyisége' ~ 1,
    value == 'album / koncert száma' ~ 10,
    value == 'NINCS NEKI' ~ 0,
    value == 'NEM MÁSOLT' ~ 0,
    value == 'NEM VÁSÁROLT' ~ 0,
    TRUE ~ NA_real_
  ))

unique (acquisition_units$value )

acquisition_units <- cap19_raw %>%
  select ( uniqid, starts_with("acq") ) %>%
  select ( -contains("reality")) %>%
  gather ( var_name, value, !! 2:ncol(.) ) %>%
  filter ( nchar(value) > 8 )


## Eszközellátottság feldolgozása -------

recode_durables <- function(x) {

  tmp <- case_when (
    x == "4-5" ~ "4.5",
    str_sub(x, 1,1) == "t" ~ "6.5",
    str_sub(x, 1,1) == "N" ~ NA_character_,
    TRUE ~ as.character (x)
    )

  as.numeric(tmp)
}

durables  <- cap19_raw %>%
  select (  uniqid, contains ('durable') ) %>%
  gather ( var_name, value, !! 2:ncol(.)) %>%
  mutate ( value = recode_durables(value) ) %>%
  spread ( var_name, value )


#### Zenehallgatás névleges óraszáma  -----
#### Figyelem, a youtube órák összesítve és bontva is szerepelnek!

listen_music_raw  <- select ( cap19_raw, uniqid,
                             starts_with ( "listen_")) %>%
  select ( uniqid, contains ("_raw") )  %>%
  gather(  media, raw_value, !!2:ncol(.)) %>%
  mutate ( media = gsub("listen_hours_raw_", "", media )) %>%
  mutate ( raw_value = case_when (
    is.na(raw_value) ~ 0,
    raw_value > 9997 ~ 0,
    TRUE ~ as.numeric(raw_value)
    ))
unique (listen_music_raw$media )

listen_music_units <- select ( cap19_raw, uniqid,
                               starts_with ( "listen_")) %>%
  select ( uniqid, contains("hours"), -contains("raw"),
           -contains("review"), -contains("total")) %>%
  mutate_all ( ~gsub("NEM HALLGAT", 0, .)) %>%
  mutate_all ( ~gsub("NEM HASZNÁL", 0, .)) %>%
  mutate_all ( ~gsub("havi", 12, .)) %>%
  mutate_all ( ~gsub("napi", 250, .)) %>%
  mutate_all ( ~gsub("heti", 50, .)) %>%
  mutate_all ( ~gsub("NT-NV", NA, .)) %>%
  gather ( media, listen_unit, !!2:ncol(.)) %>%
  mutate ( media = gsub("listen_hours_", "", media ))  %>%
  mutate ( listen_unit = ifelse(is.na(listen_unit),
                                yes = as.numeric(0),
                                no = as.numeric(as.character(listen_unit)) ))


listen_music_combined <- left_join (
  listen_music_units, listen_music_raw,
  by = c("uniqid", "media") ) %>%
  mutate ( new_var = paste0("listen_music_", media)) %>%
  mutate ( value = listen_unit*raw_value )

listen_music <- listen_music_combined %>%
  select ( uniqid, new_var, value ) %>%
  spread ( new_var, value )

listen_music_2019 <- left_join ( weights, listen_music, by = 'uniqid')

summary ( listen_music )
saveRDS( listen_music_2019, file.path(uhd_folder, "data", "2019","listen_music_2019.rds") )



## Zene tárolása  -----------------
## Zeneszámokban megadva (1 album = 10 zeneszám)

store_music_raw  <- select ( cap19_raw, uniqid,
                             starts_with ( "store_")) %>%
  select ( uniqid, contains ("music_raw") )  %>%
  gather(  media, raw_value, !!2:ncol(.)) %>%
  mutate ( media = gsub("store_music_raw_", "", media )) %>%
  mutate ( raw_value = ifelse(is.na(raw_value), 0, raw_value ))


store_music_units <- cap19_raw %>%
  select ( uniqid, starts_with ( "store_")) %>%
  select ( uniqid, contains("music")) %>%
  select ( -contains("raw"), -contains("domestic")) %>%
  mutate_all ( ~gsub("NEM TÁROL", 0, .)) %>%
  mutate_all ( ~gsub("album / koncert száma", 10, .)) %>%
  mutate_all ( ~gsub("zeneszámok mennyisége", 1, .)) %>%
  mutate_all ( ~gsub("NT-NV", NA, .)) %>%
  gather ( media, music_unit, !!2:ncol(.)) %>%
  mutate ( media = gsub("store_music_", "", media ))  %>%
  mutate ( music_unit = ifelse(is.na(music_unit),
                               yes = as.numeric(0),
                               no = as.numeric(as.character(music_unit)) ))


store_music_combined <- left_join ( store_music_units, store_music_raw,
                                    by = c("uniqid", "media") ) %>%
  mutate ( new_var = paste0("store_music_", media)) %>%
  mutate ( value = music_unit*raw_value )

store_music <- store_music_combined %>%
  select ( uniqid, new_var, value ) %>%
  spread ( new_var, value )

store_music_2019 <- left_join ( weights, store_music, by = 'uniqid')
saveRDS( store_music_2019, file.path("data", "2019","store_music_2019.rds"))



#mean ( store_music_rates$paid_rate , na.rm=TRUE)

## Audiovizuális tartalmak tárolása  -------------------
## Itt nincsenek mértékegységek, de eltérőek a tartalmak

# long form nyers adatok
store_av_raw  <- cap19_raw %>%
  select ( uniqid, starts_with ( "store_")) %>%
  select ( -contains ("music"), -contains("audiobook"), -contains("rate"),
           -store_4583, -store_4582 )  %>%
  gather(  media, raw_value, !!2:ncol(.)) %>%
  mutate ( content = word(media, 2,2, sep = "_")) %>%
  mutate ( device  = word(media, 3, -1, sep="_")) %>%
  mutate ( value = case_when (
    raw_value > 99997 ~ NA_real_,
    is.na(raw_value) ~  NA_real_,
    TRUE ~ as.numeric(raw_value)))

# széles feldolgozott adatok
store_av <- store_av_raw %>%
  select ( uniqid, media, value ) %>%
  spread ( media, value )

summary ( store_av )
store_av_2019 <- left_join ( weights, store_av, by = 'uniqid')

saveRDS( store_av, file.path("data", "2019","store_av_2019.rds"))

## Zene rátöltése -------------------------------------------
## Zeneszámokban megadva (1 album = 10 zeneszám)

download_music_raw  <- cap19_raw %>%
  select ( uniqid, starts_with ( "download_music")) %>%
  select ( uniqid, contains ("music_raw") )  %>%
  gather(  media, raw_value, !!2:ncol(.)) %>%
  mutate ( media = gsub("download_music_raw_", "", media )) %>%
  mutate ( value = case_when (
    raw_value > 9997 ~  NA_real_,
    is.na(raw_value) ~  NA_real_,
    TRUE ~ as.numeric(raw_value)))

download_music_units <- cap19_raw %>%
  select ( uniqid,  starts_with ( "download_")) %>%
  select ( uniqid, contains("music")) %>%
  select ( -contains("raw"),
           -contains("rate"), -contains("reality")) %>%
  mutate_all ( ~gsub("NEM MÁSOLT", 0, .)) %>%
  mutate_all ( ~gsub("album / koncert száma", 10, .)) %>%
  mutate_all ( ~gsub("zeneszámok mennyisége", 1, .)) %>%
  mutate_all ( ~gsub("NT-NV", NA, .)) %>%
  gather ( media, music_unit, !!2:ncol(.)) %>%
  mutate ( media = gsub("download_music_", "", media ))  %>%
  mutate ( music_unit = ifelse(test = is.na(music_unit),
                               yes = as.numeric(0),
                               no = as.numeric(as.character(music_unit))
                               )
           )

#long form, mértékegységgel korrigált
download_music_combined <- left_join (
  download_music_units, download_music_raw, by = c("uniqid", "media") ) %>%
  mutate ( new_var = paste0("download_music_", media)) %>%
  mutate ( value = music_unit*raw_value )

# széles forma, mértékegységgel korrigált
download_music <- download_music_combined %>%
  select ( uniqid, new_var, value ) %>%
  spread ( new_var, value )

is_download_music <- download_music %>%
  mutate ( is_download_music = rowSums(.[,-1], na.rm=TRUE)) %>%
  mutate ( is_download_music = ifelse ( is_download_music >0, 1, 0)) %>%
  select ( uniqid, is_download_music ) %>%
  left_join ( weights, by = 'uniqid')

sum(is_download_music$is_download_music)
sum(is_download_music$is_download_music*is_download_music$proj_weight_10)

summary ( download_music )
download_music_2019 <- left_join ( weights,
                                   download_music, by = 'uniqid' )
saveRDS( download_music_2019, file.path("data", "2019","download_music_2019.rds"))


## Zenei másolási arányok ----------------------------------
##
download_music_rates  <- cap19_raw %>%
  select ( uniqid, starts_with ("download_music")) %>%
  select ( uniqid, contains ("rate"))

download_av_rates <- cap19_raw %>%
  select ( uniqid, starts_with ("download_av")) %>%
  select ( uniqid, contains ("rate")) %>%
  gather ( names, values, starts_with("download") ) %>%
  mutate ( names = gsub("download_av_|_rate", "", names ) ) %>%
  mutate ( content  = stringr::word(names, 1, 1, sep = '_' )) %>%
  mutate ( type     = stringr::word(names, 2, -1, sep = '_' )) %>%
  select ( -names ) %>%
  mutate ( values = ifelse ( values > 100, NA, values )) %>%
  spread ( type, values ) %>%
  mutate ( missings = rowSums(is.na(.)) ) %>%
  mutate ( paid = ifelse ( is.na(paid) & missings < 3, 0, paid),
           pcr  = ifelse ( is.na(pcr) & missings < 3, 0, pcr ),
           torrent = ifelse ( is.na(torrent) & missings < 3, 0, torrent)) %>%
  mutate ( total = paid + pcr + torrent ) %>%
  filter ( missings < 3)


download_av_rates <- process_numeric_table(
  dat = cap19_raw ,
  name_starts = 'download_av',
  name_contains = 'rate')  %>%
  filter ( complete.cases(.))
  #spread ( type, values ) %>%
  #group_by ( content, type )
  #summarize ( values = mean ( values, na.rm=TRUE ))

  download_music_rates  <- cap19_raw %>%
  select ( uniqid, starts_with ("download_music")) %>%
  select ( uniqid, contains ("rate")) %>%
  select ( -contains ("domestic")) %>%
  gather ( names, values, starts_with("download") ) %>%
  mutate ( names = gsub("download_music_|_rate", "", names ) ) %>%
  mutate ( values = ifelse ( values > 100, NA, values )) %>%
  spread ( names, values ) %>%
  mutate ( missings = rowSums(is.na(.)) ) %>%
  mutate ( paid = ifelse ( is.na(paid) & missings < 3, 0, paid),
           pcr  = ifelse ( is.na(pcr) & missings < 3, 0, pcr ),
           torrent = ifelse ( is.na(torrent) & missings < 3, 0, torrent)) %>%
  mutate ( total = paid + pcr + torrent ) %>%
  filter ( missings < 3)


#download_music_domestic_rate <- download_music_rates %>%
#  select ( uniqid, download_music_domestic_rate)

#download_music_source_rate <- download_music_rates %>%
#  select ( uniqid, -download_music_domestic_rate )

## AV tartalmak rátöltése ----------------------------------
## Nincsen mértékegység, de háromféle tartalom van.

#long form
download_av_2019  <- cap19_raw %>%
  select (  uniqid, starts_with ( "download_")) %>%
  select ( -contains ("music"), -contains("rate") )  %>%
  gather(  device, values, !!2:ncol(.)) %>%
  mutate ( device = gsub("download_", "", device )   ) %>%
  mutate ( values = ifelse( is.na(values), 0, values ),
           values = ifelse( values > 99900, 0, values)) %>%
  mutate ( content = case_when (
    grepl ( "film_",   device )  ~ 'film',
    grepl ( "series_", device )  ~ 'series',
    grepl ( "tvprg_",  device )  ~ 'tvprg'
    )) %>%
  mutate ( device = gsub("film_|series_|tvprg_", "", device) )

#széles forma
download_av <- download_av_2019  %>%
  select ( uniqid, content, device, values ) %>%
  mutate ( var_name = paste0("download_", content, "_", device )) %>%
  select ( uniqid, var_name, values ) %>%
  spread ( var_name, values)

summary ( download_av)

is_download_av <- download_av %>%
  mutate ( is_download_av = rowSums(.[,-1], na.rm=TRUE)) %>%
  mutate ( is_download_av = ifelse ( is_download_av > 0, 1, 0)) %>%
  select ( uniqid, is_download_av ) %>%
  left_join ( weights, by = 'uniqid')

sum(is_download_av$is_download_av*is_download_av$proj_weight_10)


download_av_2019 <- left_join ( weights,
                                   download_av, by = 'uniqid' )
summary ( download_av_2019 )
saveRDS ( download_av_2019,
          file.path(uhd_folder, "data", "2019", "download_av_2019.rds"))


#a tisztított fájl a uniqid alapján balról jobbra egyesítve

View ( is_film_or_music  )

cap_19_hu <- weights %>%
  left_join ( binary_vars, by = 'uniqid' ) %>%
  left_join ( hypothetical_interest, by = 'uniqid' ) %>%
  left_join ( is_film_or_music, by = 'uniqid') %>%
  left_join ( durables, by = 'uniqid') %>%
  left_join ( listen_music, by = 'uniqid') %>%
  left_join ( store_music, by = 'uniqid') %>%
  left_join ( watch_av, by = 'uniqid') %>%
  left_join ( big_five, by = 'uniqid') %>%
  left_join ( hypothetical_values, by = 'uniqid') %>%
  left_join ( download_music, by = 'uniqid' ) %>%
  left_join ( download_av, by = 'uniqid') %>%
  left_join ( audiobooks, by = 'uniqid' ) %>%
  left_join ( visits, by = 'uniqid') %>%
  left_join ( activities, by = 'uniqid') %>%
  left_join ( artistic_activities, by = 'uniqid') %>%
  left_join ( is_film_or_music, by = 'uniqid')

saveRDS( cap_19_hu, file.path ( 'data', '2019', 'cap19_hu.rds'))

##ellenőrző kód:
if ( nrow (cap_19_hu) != nrow (cap19_raw) ) warning("A megfigyelések száma nem egyezik")

##nem maradtak bent 99998, 99999 értékek
max_values <- cap_19_hu %>% summarize_if ( is.numeric, max, na.rm=TRUE) %>%
  gather ( var_name, max_values )

## az egyszerűbb kezeletőség végett az egyes változóblokkok önállóan is
## el vannak mentve.

saveRDS( weights, file.path(uhd_folder,
                            "data", "2019", "cap_19_hu_weights.rds") )


## többes választás kérdések, súlyokkal együtt elmentve

cap_19_hu_multiple_choice <-   weights  %>%
  left_join ( multiple_choice, by = 'uniqid' )

saveRDS( cap_19_hu_multiple_choice, file.path("data", "cap_19_hu_multiple_choice.rds") )
saveRDS( cap_19_hu_multiple_choice, file.path(uhd_folder, "data", "2019", "cap_19_hu_multiple_choice.rds") )


visit_table  <- visits %>%
  select ( uniqid, visit_concert, visit_disco, visit_cinema ) %>%
  left_join ( weights, by = 'uniqid' )

saveRDS( visit_table, file.path(cap_folder, "data", "visits_19.rds") )
saveRDS( visit_table, file.path(uhd_folder, "data", "2019", "visits_19.rds") )


demografia <- cap19_raw %>%
  select ( uniqid, age_exact, age_education,
           difficulty_bills, esomar )

cap19_raw %>% contains ( "difficulty")

summary ( cap_19_hu_multiple_choice )

summary ( cap_19_hu )

demografia <- cap19_raw %>%
  select ( uniqid, gender,
           proj_weight_10, weight_10,
           age_exact, year_birth,
           age_education, age_education_raw, education_highest,
           occupation, occupation_paid,
           subjective_urbanization, municipality,
           difficulty_bills ) %>%
  left_join ( self_placement_society, by = 'uniqid') %>%
  left_join ( big_five, by = 'uniqid' )

saveRDS ( demografia, file.path(uhd_folder, "data", "2019", "demografia_19.rds")  )

save ( download_music_2019, download_av_2019,
       visits, activities,  artistic_activities,
       store_music,
       listen_music,
       download_music,
       watch_av,
       hypothetical_values,
       binary_vars,
       cap_19_hu_multiple_choice,
       durables,
       big_five,
       weights,
       file = file.path('data', '2019', 'cap19_hu.rda' ))


save ( download_music_2019, download_av_2019,
       visits, activities,  artistic_activities,
       store_music,
       listen_music,
       download_music,
       watch_av,
       hypothetical_values,
       binary_vars,
       cap_19_hu_multiple_choice,
       durables,
       big_five,
       weights,
       file = file.path(uhd_folder, 'data', '2019', 'cap19_hu.rda' ))
