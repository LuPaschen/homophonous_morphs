# Script 1/3 for Paschen (2025): Acoustic disambiguation of homophones is exceptional. Journal of Linguistics. https://doi.org/10.1017/S0022226725100777

### 0) Load libraries
library(tidyverse)
library(here)



### 1) Read data
# Read CSV files from DoReCo 2.0 [can be downloaded from https://doreco.huma-num.fr or https://doi.org/10.34847/nkl.7cbfq779]
doreco_csv_dir = here("data","ph_csv")
doreco_csv_files <- list.files(path = doreco_csv_dir, pattern = "\\_ph.csv$", full.names = TRUE, recursive = TRUE)
doreco_csv_data <- map_df(doreco_csv_files, ~read_csv(.x, col_types = cols(speaker = col_character())))

# Read custom-made gloss equivalence tables
gl_csv_dir = here("data","gl_csv")
gl_csv_files <- list.files(path = gl_csv_dir, pattern = "Gloss equivalence sets", full.names = TRUE, recursive = TRUE)
gl_csv_data <- map_df(gl_csv_files, readr::read_csv)



### 2) Add new columns and remove columns not needed for further analysis
vowels <- c("i","y","1","}","M","u","I","Y","U","e","2","@","8","7","o","E","9","3","V","O","{","6","a","&","A","Q")
voiced_consonants <- c("m","F","n","J","N","b","d","g","G","B","v","D","z","Z","j","R","r","4","l","5","L","w")
voiceless_consonants <- c("p","t","c","k","q",">","?","f","T","s","S","C","x","X","H","h","K","|","!","=")
doreco_data_preprocessed <- doreco_csv_data %>%
  # Remove columns not needed for analysis
  select(-ref,-tx,-ft,-core_extended,-"doreco-mb-algn",-"mc-zero",-refind,-isnref) %>%
  # Phone duration
  mutate(ph_duration = end - start) %>%
  # Segmental context 1: Segments to the left and to the right (per morph)
  mutate(lead_ph = lead(ph),
         lag_ph = lag(ph)) %>%
  group_by(lang, wd_ID) %>%
  mutate(
    segmental_context_raw_r = ifelse(row_number() == n(), "#", lead_ph),
    segmental_context_raw_l = ifelse(row_number() == 1, "#", lag_ph)) %>%
  group_by(lang, mb_ID) %>%
  mutate(
    segmental_context_raw_r = last(na.omit(segmental_context_raw_r)),
    segmental_context_raw_l = first(na.omit(segmental_context_raw_l)),
  ) %>%
  # Segmental context 2: Convert to general categories
  mutate(segmental_context_r = case_when(
    segmental_context_raw_r == "#" ~ "#",
    substr(segmental_context_raw_r, 1, 1) %in% voiced_consonants ~ "D",
    substr(segmental_context_raw_r, 1, 1) %in% voiceless_consonants ~ "T",
    substr(segmental_context_raw_r, 1, 1) %in% vowels ~ "V",
    TRUE ~ NA
  ),
  segmental_context_l = case_when(
    segmental_context_raw_l == "#" ~ "initial",
    TRUE ~ "non-initial"
  ),
  segmental_context = paste(segmental_context_l, segmental_context_r, sep = "__")
  ) %>%
  # Morph duration, morph type, base morph form
  group_by(lang, mb_ID) %>%
  mutate(mb_duration = last(end) - first(start),
         mb_size = n(),
         mb_raw = str_replace_all(tolower(mb), "[\\-=~]", ""),
         mt = case_when(
           mb == "" ~ NA,
           startsWith(mb, "<") ~ NA,
           startsWith(mb, "*") ~ NA,
           startsWith(mb, "-") & endsWith(mb, "-") ~ "infix",
           startsWith(mb, "-") ~ "suffix",
           endsWith(mb, "-") ~ "prefix",
           startsWith(mb, "~") ~ "suffix",
           endsWith(mb, "~") ~ "prefix",
           startsWith(mb, "=") ~ "enclitic",
           endsWith(mb, "=") ~ "proclitic",
           is.na(mb) ~ NA,
           TRUE ~ "root")) %>%
  # Word duration, word size
  group_by(lang, wd_ID) %>%
  mutate(wd_duration = last(end) - first(start),
         wd_size_raw = n(),
         wd_size = wd_size_raw - mb_size) %>%
  # IPU's, IPU-final position
  group_by(lang, file, speaker) %>%
  mutate(is_pause = ifelse(ph == "<p:>", TRUE, FALSE),
         ipu_ID_perspeaker = cumsum(is_pause),
         position_in_ipu = ifelse(lead(ph) == "<p:>", "final", "non-final")) %>%
  ungroup() %>%
  mutate(change = c(1, diff(ipu_ID_perspeaker) != 0),
         ipu_ID = cumsum(change)) %>%
  # Speech rate (not counting pauses, labels, and adjusting for auto-correlation)
  group_by(ipu_ID) %>%
  mutate(ipu_duration = last(end) - first(start),
         number_pauses_and_labels = sum(grepl("<p:>|^<<", wd), na.rm = TRUE),
         total_duration_pauses_and_labels = sum(ph_duration[grepl("<p:>|^<<", wd)], na.rm = TRUE),
         adjusted_ipu_duration = ipu_duration - total_duration_pauses_and_labels - mb_duration,
         speech_rate = (n()-number_pauses_and_labels-mb_size) / adjusted_ipu_duration) %>%
  # Remove temporary columns
  select(-mb_size,-wd_size_raw,-is_pause,-change,-ipu_ID_perspeaker,-number_pauses_and_labels,-total_duration_pauses_and_labels,-adjusted_ipu_duration,-lead_ph,-lag_ph) %>% # 
  ungroup()



### 3) Standardizing glosses based on pre-defined gloss equivalence sets
# Read gloss equivalence sets
gl_pivot <- gl_csv_data %>%
  pivot_longer(cols = -c(gl, lang), names_to = "variable", values_to = "value")

matches <- gl_pivot %>%
  filter(value %in% doreco_data_preprocessed$gl) %>%
  select(-variable) %>%
  distinct(lang, value, .keep_all = TRUE)

doreco_data_gloss_equivalence_applied <- doreco_data_preprocessed %>%
  left_join(matches, by = c("gl" = "value", "lang")) %>%
  mutate(gl_final = coalesce(gl.y, as.character(gl))) %>%
  select(-gl,-gl.y) %>%
  rename(gl = gl_final)



### 4) Further preprocessing related to homophones: Filtering, adding frequency and homogeneity measure
homophone_data <- doreco_data_gloss_equivalence_applied %>%
  # Remove pauses
  filter(!grepl("<p:>", wd)) %>%
  # Word frequency
  group_by(lang, wd_ID) %>%
  mutate(word_and_glosses = paste(wd, paste(unique(gl), collapse = " "), sep = " ")) %>%   
  group_by(lang, word_and_glosses) %>%
  mutate(wd_freq = n_distinct(wd_ID)) %>%
  # Remove labeled content
  filter(!grepl("^<<", wd)) %>%
  # Remove morphs and glosses with filler content
  filter(mb != "****", gl != "****", gl != "NC", gl != "nc") %>%
  # Remove rows containing NAs (may arise with monomorphemic IPU's)
  filter(!is.na(speech_rate)) %>%
  # Remove morph/gloss pairs with fewer than 10 occurrences
  group_by(lang, mb_raw, gl) %>%
  mutate(mb_freq = n_distinct(mb_ID)) %>%
  filter(mb_freq >= 10) %>%
  # Exclude glosses that combine various unrelated meanings
  filter(!(lang == "savo1255" & (gl %in% c("DET.SG.M/DET.PL", "stand/INGR")))) %>%
  filter(!(lang == "ruul1235" & (gl %in% c("1-")))) %>%
  filter(!(lang == "sanz1248" & (gl %in% c("-PL")))) %>%
  filter(!(lang == "nort2641" & (gl %in% c("ADP", "POP")))) %>%
  # Determine homogeneity in morph types
  group_by(lang, mb_raw) %>%
  mutate(homogeneity_mt = if_else(n_distinct(mt) == 1, "Homogeneous", "Heterogeneous")) %>%
  # Remove morphs with only one distinct gloss
  mutate(n_homophones = n_distinct(gl)) %>%
  filter(n_homophones > 1) %>%
  # Restructure dataframe to one row = one morph
  group_by(lang, mb_ID) %>%
  filter(ph_ID == last(ph_ID)) %>%
  ungroup()



### 5) Replace glottocodes with human-readable language names
glottocodes <- c("anal1239", "apah1238", "arap1274", "bain1259", "beja1238", "bora1263", "cabe1245", "cash1254", "dolg1241", "even1259", "goem1240", "goro1270", "hoch1243", "jeha1242", "jeju1234", "kaka1265", "kama1351", "kark1256", "komn1238", "ligh1234", "lowe1385", "movi1243", "ngal1292", "nisv1234", "nngg1234", "nort2641", "nort2875", "orko1234", "pnar1238", "port1286", "resi1247", "ruul1235", "sadu1234", "sanz1248", "savo1255", "sout2856", "sout3282", "stan1290", "sumi1235", "svan1243", "taba1259", "teop1238", "texi1237", "trin1278", "tsim1256", "urum1249", "vera1241", "warl1254", "yong1270", "yuca1254", "yura1255", "toto1304", "guri1247")
language_names <- c("Anal", "Yali", "Arapaho", "Baïnounk Gubëeher", "Beja", "Bora", "Cabécar", "Cashinahua", "Dolgan", "Evenki", "Goemai", "Gorwaa", "Hoocąk", "Jahai", "Jejuan", "Kakabe", "Kamas", "Tabaq", "Komnzo", "Light Warlpiri", "Lower Sorbian", "Movima", "Dalabon", "Nisvai", "Nǁng", "Northern Kurdish (Kurmanji)", "Northern Alta", "Fanbyak", "Pnar", "Daakie", "Resígaro", "Ruuli", "Sadu", "Sanzhi Dargwa", "Savosavo", "Nafsan (South Efate)", "English (Southern England)", "French (Swiss)", "Sümi", "Svan", "Tabasaran", "Teop", "Texistepec Popoluca", "Mojeño Trinitario", "Asimjeeg Datooga", "Urum", "Vera'a", "Warlpiri", "Yongning Na", "Yucatec Maya", "Yurakaré", "Totoli", "Gurindji")
mapping_vector <- setNames(language_names, glottocodes)
homophone_data$lang <- mapping_vector[homophone_data$lang]



### 6) Save object
save.image(here("01_Preprocessing.RData"))

