library(dplyr)
library(stringr)
library(tidygeocoder)
library(purrr)
library(geosphere)
library(readr)
library(writexl)

# Calculate uncertainty from OSM bounding box
calc_uncertainty_from_bbox <- function(bbox) {
  
  if (is.null(bbox) || length(bbox) < 4 || any(is.na(bbox))) {
    return(NA_real_)
  }
  
  bbox <- as.numeric(bbox)
  
  south <- bbox[1]
  north <- bbox[2]
  west  <- bbox[3]
  east  <- bbox[4]
  
  diagonal_m <- geosphere::distHaversine(
    p1 = c(west, south),
    p2 = c(east, north)
  )
  
  round(diagonal_m / 2)
}


# Standardize geocoding output
standardize_geocode_result <- function(df, uncertainty_col) {
  
  if (!"boundingbox" %in% names(df)) {
    df$boundingbox <- vector("list", nrow(df))
  }
  
  if (!"display_name" %in% names(df)) {
    df$display_name <- NA_character_
  }
  
  if (!"class" %in% names(df)) {
    df$class <- NA_character_
  }
  
  if (!"type" %in% names(df)) {
    df$type <- NA_character_
  }
  
  df %>%
    mutate(
      "{uncertainty_col}" := map_dbl(
        boundingbox,
        calc_uncertainty_from_bbox
      )
    )
}


# Clean locality text
clean_locality_text <- function(x) {
  
  x %>%
    str_replace_all("\\.", " ") %>%
    str_replace_all(",", " ") %>%
    str_replace_all(":", " ") %>%
    str_replace_all(";", " ") %>%
    str_replace_all("\\(", " ") %>%
    str_replace_all("\\)", " ") %>%
    str_replace_all("\\[", " ") %>%
    str_replace_all("\\]", " ") %>%
    str_replace_all("/", " ") %>%
    str_replace_all("-", " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_squish()
}


# Simplify long locality strings
simplify_locality_text <- function(x) {
  
  x_clean <- clean_locality_text(x)
  
  x_clean <- str_remove_all(
    x_clean,
    regex(
      "\\b(km|kil[oó]metro|carretera|road|near|cerca de|trocha|quebrada|qbrda|qda|fundo|comunidad|caser[ií]o|estaci[oó]n|bosque|bordes|rocosos|minutos|hp|hc)\\b",
      ignore_case = TRUE
    )
  )
  
  x_clean <- str_remove_all(x_clean, "\\b[0-9]+\\b")
  x_clean <- str_squish(x_clean)
  
  x_clean <- ifelse(
    str_count(x_clean, "\\S+") > 5,
    str_extract(x_clean, "^(\\S+\\s+){0,4}\\S+"),
    x_clean
  )
  
  str_squish(x_clean)
}


# Geocode records without coordinates
geocode_missing_records <- function(data,
                                    id_col = "occurrenceID",
                                    country_col = "country",
                                    state_col = "stateProvince",
                                    locality_col = "locality",
                                    lat_col = "decimalLatitude",
                                    lon_col = "decimalLongitude") {
  
  data <- data %>%
    mutate(.row_id_original = row_number()) %>%
    rename_with(
      ~ paste0("original_", .x),
      .cols = any_of(c("class", "type"))
    )
  
  to_geocode <- data %>%
    mutate(
      lat_text = str_squish(as.character(.data[[lat_col]])),
      lon_text = str_squish(as.character(.data[[lon_col]])),
      
      lat_missing = is.na(.data[[lat_col]]) |
        lat_text == "" |
        lat_text %in% c("NA", "N/A", "NULL", "null", "NaN"),
      
      lon_missing = is.na(.data[[lon_col]]) |
        lon_text == "" |
        lon_text %in% c("NA", "N/A", "NULL", "null", "NaN"),
      
      needs_geocode = lat_missing | lon_missing,
      
      locality_raw = str_squish(as.character(.data[[locality_col]])),
      state_raw = str_squish(as.character(.data[[state_col]])),
      country_raw = str_squish(as.character(.data[[country_col]])),
      
      locality_raw = na_if(locality_raw, ""),
      state_raw = na_if(state_raw, ""),
      country_raw = na_if(country_raw, ""),
      
      locality_clean = clean_locality_text(locality_raw),
      locality_simple = simplify_locality_text(locality_raw),
      
      query_locality = pmap_chr(
        list(locality_raw, state_raw, country_raw),
        ~ str_squish(paste(na.omit(c(...)), collapse = ", "))
      ),
      
      query_clean = pmap_chr(
        list(locality_clean, state_raw, country_raw),
        ~ str_squish(paste(na.omit(c(...)), collapse = ", "))
      ),
      
      query_simple = pmap_chr(
        list(locality_simple, state_raw, country_raw),
        ~ str_squish(paste(na.omit(c(...)), collapse = ", "))
      ),
      
      query_state = pmap_chr(
        list(state_raw, country_raw),
        ~ str_squish(paste(na.omit(c(...)), collapse = ", "))
      )
    ) %>%
    filter(needs_geocode) %>%
    filter(
      query_locality != "" |
        query_clean != "" |
        query_simple != "" |
        query_state != ""
    )
  
  message("Records to geocode: ", nrow(to_geocode))
  message("Distinct original locality queries: ", n_distinct(to_geocode$query_locality))
  message("Distinct clean locality queries: ", n_distinct(to_geocode$query_clean))
  message("Distinct simplified locality queries: ", n_distinct(to_geocode$query_simple))
  message("Distinct state/country queries: ", n_distinct(to_geocode$query_state))
  
  geo_locality <- to_geocode %>%
    distinct(query_locality, .keep_all = TRUE) %>%
    geocode(
      address = query_locality,
      method = "osm",
      lat = lat_locality,
      long = lon_locality,
      full_results = TRUE
    ) %>%
    standardize_geocode_result("uncertainty_locality_m") %>%
    select(
      query_locality,
      lat_locality,
      lon_locality,
      display_name_locality = display_name,
      class_locality = class,
      type_locality = type,
      uncertainty_locality_m
    )
  
  failed_locality <- to_geocode %>%
    left_join(geo_locality, by = "query_locality") %>%
    filter(is.na(lat_locality) | is.na(lon_locality))
  
  geo_clean <- failed_locality %>%
    distinct(query_clean, .keep_all = TRUE) %>%
    geocode(
      address = query_clean,
      method = "osm",
      lat = lat_clean,
      long = lon_clean,
      full_results = TRUE
    ) %>%
    standardize_geocode_result("uncertainty_clean_m") %>%
    select(
      query_clean,
      lat_clean,
      lon_clean,
      display_name_clean = display_name,
      class_clean = class,
      type_clean = type,
      uncertainty_clean_m
    )
  
  failed_clean <- failed_locality %>%
    left_join(geo_clean, by = "query_clean") %>%
    filter(is.na(lat_clean) | is.na(lon_clean))
  
  geo_simple <- failed_clean %>%
    distinct(query_simple, .keep_all = TRUE) %>%
    geocode(
      address = query_simple,
      method = "osm",
      lat = lat_simple,
      long = lon_simple,
      full_results = TRUE
    ) %>%
    standardize_geocode_result("uncertainty_simple_m") %>%
    select(
      query_simple,
      lat_simple,
      lon_simple,
      display_name_simple = display_name,
      class_simple = class,
      type_simple = type,
      uncertainty_simple_m
    )
  
  failed_simple <- failed_clean %>%
    left_join(geo_simple, by = "query_simple") %>%
    filter(is.na(lat_simple) | is.na(lon_simple))
  
  geo_state <- failed_simple %>%
    distinct(query_state, .keep_all = TRUE) %>%
    geocode(
      address = query_state,
      method = "osm",
      lat = lat_state,
      long = lon_state,
      full_results = TRUE
    ) %>%
    standardize_geocode_result("uncertainty_state_m") %>%
    select(
      query_state,
      lat_state,
      lon_state,
      display_name_state = display_name,
      class_state = class,
      type_state = type,
      uncertainty_state_m
    )
  
  geo_combined <- to_geocode %>%
    left_join(geo_locality, by = "query_locality") %>%
    left_join(geo_clean, by = "query_clean") %>%
    left_join(geo_simple, by = "query_simple") %>%
    left_join(geo_state, by = "query_state") %>%
    mutate(
      geocode_lat_final = case_when(
        !is.na(lat_locality) ~ lat_locality,
        !is.na(lat_clean) ~ lat_clean,
        !is.na(lat_simple) ~ lat_simple,
        !is.na(lat_state) ~ lat_state,
        TRUE ~ NA_real_
      ),
      
      geocode_lon_final = case_when(
        !is.na(lon_locality) ~ lon_locality,
        !is.na(lon_clean) ~ lon_clean,
        !is.na(lon_simple) ~ lon_simple,
        !is.na(lon_state) ~ lon_state,
        TRUE ~ NA_real_
      ),
      
      geocode_uncertainty_final_m = case_when(
        !is.na(lat_locality) ~ uncertainty_locality_m,
        !is.na(lat_clean) ~ uncertainty_clean_m,
        !is.na(lat_simple) ~ uncertainty_simple_m,
        !is.na(lat_state) ~ uncertainty_state_m,
        TRUE ~ NA_real_
      ),
      
      geocode_level = case_when(
        !is.na(lat_locality) ~ "locality_original",
        !is.na(lat_clean) ~ "locality_clean",
        !is.na(lat_simple) ~ "locality_simple",
        !is.na(lat_state) ~ "stateProvince",
        TRUE ~ "not_geocoded"
      ),
      
      geocode_display_name = case_when(
        geocode_level == "locality_original" ~ display_name_locality,
        geocode_level == "locality_clean" ~ display_name_clean,
        geocode_level == "locality_simple" ~ display_name_simple,
        geocode_level == "stateProvince" ~ display_name_state,
        TRUE ~ NA_character_
      ),
      
      geocode_class = case_when(
        geocode_level == "locality_original" ~ class_locality,
        geocode_level == "locality_clean" ~ class_clean,
        geocode_level == "locality_simple" ~ class_simple,
        geocode_level == "stateProvince" ~ class_state,
        TRUE ~ NA_character_
      ),
      
      geocode_type = case_when(
        geocode_level == "locality_original" ~ type_locality,
        geocode_level == "locality_clean" ~ type_clean,
        geocode_level == "locality_simple" ~ type_simple,
        geocode_level == "stateProvince" ~ type_state,
        TRUE ~ NA_character_
      ),
      
      geocode_quality = case_when(
        geocode_level == "not_geocoded" ~ "not_geocoded",
        
        geocode_level %in% c("locality_original", "locality_clean", "locality_simple") &
          !is.na(geocode_uncertainty_final_m) &
          geocode_uncertainty_final_m <= 5000 ~ "good_locality",
        
        geocode_level %in% c("locality_original", "locality_clean", "locality_simple") &
          !is.na(geocode_uncertainty_final_m) &
          geocode_uncertainty_final_m <= 20000 ~ "moderate_locality",
        
        geocode_level %in% c("locality_original", "locality_clean", "locality_simple") &
          !is.na(geocode_uncertainty_final_m) &
          geocode_uncertainty_final_m > 20000 ~ "poor_locality",
        
        geocode_level %in% c("locality_original", "locality_clean", "locality_simple") &
          is.na(geocode_uncertainty_final_m) ~ "locality_no_bbox",
        
        geocode_level == "stateProvince" &
          !is.na(geocode_uncertainty_final_m) ~ "coarse_stateProvince",
        
        geocode_level == "stateProvince" &
          is.na(geocode_uncertainty_final_m) ~ "coarse_stateProvince_no_bbox",
        
        TRUE ~ "check"
      )
    )
  
  data_geocoded <- data %>%
    left_join(
      geo_combined %>%
        select(
          .row_id_original,
          geocode_level,
          geocode_quality,
          geocode_lat_final,
          geocode_lon_final,
          geocode_uncertainty_final_m,
          geocode_display_name,
          geocode_class,
          geocode_type,
          query_locality,
          query_clean,
          query_simple,
          query_state
        ),
      by = ".row_id_original"
    )
  
  review <- geo_combined %>%
    select(
      any_of(id_col),
      any_of(country_col),
      any_of(state_col),
      any_of(locality_col),
      query_locality,
      query_clean,
      query_simple,
      query_state,
      geocode_level,
      geocode_quality,
      geocode_lat_final,
      geocode_lon_final,
      geocode_uncertainty_final_m,
      geocode_display_name,
      geocode_class,
      geocode_type
    )
  
  summary <- geo_combined %>%
    count(geocode_level, geocode_quality)
  
  doubtful <- review %>%
    filter(geocode_quality %in% c(
      "poor_locality",
      "locality_no_bbox",
      "coarse_stateProvince",
      "coarse_stateProvince_no_bbox",
      "not_geocoded",
      "check"
    )) %>%
    arrange(desc(geocode_uncertainty_final_m))
  
  list(
    data = data_geocoded,
    review = review,
    summary = summary,
    doubtful = doubtful
  )
}


# Read datasets
amaz <- read.csv(
  "georreferenciacion/AMAZ2024.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

assis <- read.csv(
  "georreferenciacion/assis_i_2026_literature.csv",
  sep = ";",
  stringsAsFactors = FALSE,
  check.names = FALSE
)


# Run geocoding
amaz_geo <- geocode_missing_records(amaz)
assis_geo <- geocode_missing_records(assis)


# Check summaries
amaz_geo$summary
assis_geo$summary


# Save full geocoded datasets
write.csv2(
  amaz_geo$data,
  "georreferenciacion/AMAZ2024_geocoded.csv",
  row.names = FALSE
)

write.csv2(
  assis_geo$data,
  "georreferenciacion/assis_i_2026_literature_geocoded.csv",
  row.names = FALSE
)



# Save doubtful records
write.csv2(
  amaz_geo$doubtful,
  "georreferenciacion/AMAZ2024_geocoded_doubtful.csv",
  row.names = FALSE
)

write.csv2(
  assis_geo$doubtful,
  "georreferenciacion/assis_i_2026_literature_geocoded_doubtful.csv",
  row.names = FALSE
)
