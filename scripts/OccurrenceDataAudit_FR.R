library(here)

dbPath <- "C:/EpIG_v2.0_/EpIG_volunteership/data/Occurrences_visualization/db_EpIG2_clean_all.rds"
db <- readRDS(here(dbPath))
# This produces only aggregate counts, not occurrence records or coordinates. 
# Once we see the result, we can decide whether: 
# - species reliably identifies formally named species;
# - morphospecies is explicitly present;
# - values such as SPECIES, Species, blanks, GENUS, or FAMILY require 
# normalization;
# - morphospecies must additionally be recognized from scientificName.
db |> 
  mutate(
    taxonRank_clean = taxonRank |>
      as.character() |>
      str_squish() |>
      str_to_lower(), 
    scientificName_present = 
      !is.na(scientificName) & str_squish(scientificName) != ""
  ) |>
  count(taxonRank_clean, scientificName_present, sort = TRUE)

# Next, we need to locate the morphospecies identifier: 
# This aggregate output will tell us whether accepted morphospecies are encoded 
# in verbaltimScientificName, recordNumber, or another identifier without 
# displaying the names themseleves. 

morpho_fields <- intersect(
  c(
    "genus",
    "verbatimScientificName",
    "scientificNameAuthorship",
    "occurrenceID",
    "recordNumber",
    "catalogNumber"
  ),
  names(db)
)

db |>
  mutate(
    taxonRank_clean = taxonRank |>
      as.character() |>
      str_squish() |>
      str_to_lower()
  ) |>
  filter(taxonRank_clean == "morphospecies") |>
  select(all_of(morpho_fields)) |>
  pivot_longer(
    cols = everything(),
    names_to = "Field",
    values_to = "Value",
    values_transform = list(Value = as.character)
  ) |>
  mutate(
    Value = str_squish(Value),
    Present = !is.na(Value) & Value != ""
  ) |>
  group_by(Field) |>
  summarise(
    Present_records = sum(Present),
    Completeness_percent = round(mean(Present) * 100, 1),
    Distinct_nonmissing_values = n_distinct(Value[Present]),
    .groups = "drop"
  ) |>
  arrange(desc(Completeness_percent))

# Based on the results, we can safely report:
# - 8,924 morphospecies occurrence records
# - 8,770 morphospecies records identified to genus: 98.3%
# - 237 represented genera

morpho_field_audit <- db |>
  mutate(
    taxonRank_clean = taxonRank |>
      as.character() |>
      str_squish() |>
      str_to_lower()
  ) |>
  filter(taxonRank_clean == "morphospecies") |>
  select(-taxonRank_clean) |>
  mutate(across(everything(), as.character)) |>
  pivot_longer(
    cols = everything(),
    names_to = "Field",
    values_to = "Value"
  ) |>
  mutate(
    Value = str_squish(Value),
    Present = !is.na(Value) & Value != ""
  ) |>
  group_by(Field) |>
  summarise(
    Present_records = sum(Present),
    Completeness_percent = round(mean(Present) * 100, 1),
    Distinct_nonmissing_values = n_distinct(Value[Present]),
    .groups = "drop"
  ) |>
  filter(Present_records > 0) |>
  arrange(
    desc(Completeness_percent),
    Distinct_nonmissing_values
  )

morpho_field_audit

print(morpho_field_audit, n = Inf)

# Final Validation: 
# Interpretation:
#   
# - Dataset_specific_morphotypes is the likely morphospecies richness metric.
# - Reused codes across datasets confirm that morpho_code alone is unsafe.
# - Conflicting_genera tests whether the same dataset-specific morphotype was 
# assigned to multiple genera.
# - Missing_genus identifies morphotypes that can still be counted but lack 
# genus-level placement.

morpho_validation <- db |>
  mutate(
    taxonRank_clean = taxonRank |>
      as.character() |>
      str_squish() |>
      str_to_lower(),
    datasetID = str_squish(as.character(datasetID)),
    morpho_code = str_squish(as.character(morpho_code)),
    genus = str_squish(as.character(genus))
  ) |>
  filter(taxonRank_clean == "morphospecies")

morpho_key_summary <- morpho_validation |>
  summarise(
    Records = n(),
    Global_morpho_codes = n_distinct(morpho_code),
    Dataset_specific_morphotypes =
      n_distinct(datasetID, morpho_code),
    Datasets = n_distinct(datasetID)
  )

code_reuse_summary <- morpho_validation |>
  distinct(morpho_code, datasetID) |>
  count(morpho_code, name = "Datasets_using_code") |>
  summarise(
    Codes_used_in_one_dataset =
      sum(Datasets_using_code == 1),
    Codes_reused_across_datasets =
      sum(Datasets_using_code > 1),
    Maximum_datasets_per_code =
      max(Datasets_using_code)
  )

taxonomic_consistency <- morpho_validation |>
  group_by(datasetID, morpho_code) |>
  summarise(
    Genera = n_distinct(genus[!is.na(genus) & genus != ""]),
    .groups = "drop"
  ) |>
  summarise(
    Dataset_morphotypes = n(),
    Assigned_to_one_genus = sum(Genera == 1),
    Missing_genus = sum(Genera == 0),
    Conflicting_genera = sum(Genera > 1)
  )

morpho_key_summary
code_reuse_summary
taxonomic_consistency

# We use genus as the placement where available, then family or order as fallbacks:
morpho_final_audit <- morpho_validation |>
  mutate(
    family = str_squish(as.character(family)),
    order = str_squish(as.character(order)),
    genus = na_if(genus, ""),
    family = na_if(family, ""),
    order = na_if(order, ""),
    taxonomic_anchor = case_when(
      !is.na(genus)  ~ paste0("genus:", genus),
      !is.na(family) ~ paste0("family:", family),
      !is.na(order)  ~ paste0("order:", order),
      TRUE           ~ "unplaced"
    ),
    morphotype_key = paste(
      datasetID,
      taxonomic_anchor,
      morpho_code,
      sep = "::"
    )
  )

morpho_final_summary <- morpho_final_audit |>
  summarise(
    Records = n(),
    Contributor_specific_morphotypes =
      n_distinct(morphotype_key),
    Genus_anchored =
      n_distinct(morphotype_key[taxonomic_anchor != "unplaced" &
                                  str_starts(taxonomic_anchor, "genus:")]),
    Family_fallback =
      n_distinct(morphotype_key[str_starts(taxonomic_anchor, "family:")]),
    Order_fallback =
      n_distinct(morphotype_key[str_starts(taxonomic_anchor, "order:")]),
    Taxonomically_unplaced =
      n_distinct(morphotype_key[taxonomic_anchor == "unplaced"])
  )

morpho_final_summary

# The morphospecies component is now measurable:
#   
# - 8,924 morphospecies occurrence records
# - 2,075 contributor-specific morphotypes
# - 2,029 genus-anchored morphotypes: 97.8%
# - 46 taxonomically unplaced morphotypes: 2.2%
# - No family- or order-level fallback cases

# ---------------
# This establishes the record-level denominator without prematurely claiming a 
# number of unique accepted species. The next taxonomic step will be identifying 
# the appropriate field for rolling varieties, subspecies, and forms up to species 
# level.

taxonomy_audit <- db |>
  mutate(
    taxonRank_clean = taxonRank |>
      as.character() |>
      str_squish() |>
      str_to_lower(),
    scientificName_clean = scientificName |>
      as.character() |>
      str_squish() |>
      na_if(""),
    taxonomic_group = case_when(
      taxonRank_clean %in%
        c("species", "subspecies", "variety", "form") &
        !is.na(scientificName_clean) ~
        "Formally named species-level record",
      
      taxonRank_clean == "hybrid" &
        !is.na(scientificName_clean) ~
        "Named hybrid",
      
      taxonRank_clean == "morphospecies" ~ 
        "Morphospecies record", 
      
      taxonRank_clean %in%
        c("species", "subspecies", "variety", "form", "hybrid") &
        is.na(scientificName_clean) ~
        "Expected name missing",
      
      TRUE ~
        "Unsupported or missing taxonomic rank"
    )
  )

taxonomy_record_summary <- taxonomy_audit |>
  count(taxonomic_group, name = "Records") |>
  mutate(
    Percentage = round(Records / nrow(db) * 100, 4)
  ) |>
  arrange(desc(Records))

taxonomy_record_summary

# Our audit accounts for all 1,372,136 records:
#   
# - 1,363,198 formally named species-level records: 99.3%
# - 8,924 morphospecies records: 0.65%
# - 14 named hybrid records: approximately 0.001%

# Next, we will determine whether varieties, subspecies, and forms can be 
# reliably rolled up to species without parsing scientificName manually.
names(db)[
  str_detect(
    str_to_lower(names(db)), 
    "scient|species|taxon|accepted|canonical|genus|epithet|synonym|name"
  )
]

# Let's determine which field represent the standardized EpIG taxon name and 
# whether it already reduces infra-specific records to species level.
taxon_name_fields <- intersect(
  c(
    "scientificName",
    "matchedNameEpig",
    "matchedNameRawEpig",
    "synonymEpig"
  ),
  names(db)
)

is_present <- function(x) {
  !is.na(x) & str_squish(as.character(x)) != ""
}

count_present <- function(x) {
  sum(is_present(x))
}

count_distinct_present <- function(x) {
  x <- str_squish(as.character(x))
  n_distinct(x[!is.na(x) & x != ""])
}

taxonomy_name_audit <- db |>
  mutate(
    taxonRank_clean = taxonRank |>
      as.character() |>
      str_squish() |>
      str_to_lower()
  ) |>
  filter(
    taxonRank_clean %in%
      c("species", "variety", "subspecies", "form", "hybrid")
  ) |>
  group_by(taxonRank_clean) |>
  summarise(
    Records = n(),
    across(
      all_of(taxon_name_fields),
      list(
        Present = count_present,
        Distinct = count_distinct_present
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

taxonomy_name_audit |>
  print(width = Inf)

# The inspect a few distinct mappings rather than occurrence records:
taxonomy_name_examples <- db |>
  mutate(
    taxonRank_clean = taxonRank |>
      as.character() |>
      str_squish() |>
      str_to_lower()
  ) |>
  filter(
    taxonRank_clean %in%
      c("species", "variety", "subspecies", "form", "hybrid")
  ) |>
  select(
    taxonRank_clean,
    all_of(taxon_name_fields)
  ) |>
  distinct() |>
  group_by(taxonRank_clean) |>
  slice_head(n = 5) |>
  ungroup()

taxonomy_name_examples |>
  print(n = Inf, width = Inf)

# The examples strongly indicate that scientificName is the correct species-level 
# field:
#   
# - It contains binomial species names even when taxonRank is variety, 
# subspecies, or form.
# - matchedNameRawEpig appears to preserve the originally matched name.
# - matchedNameEpig may retain an infraspecific name.
# - scientificName appears to contain the reconciled name reduced to species 
# level.
# - synonymEpig indicates whether the original matched name was accepted or 
# synonymous.

# Therefore, we should calculate formal species richness from cleaned 
# scientificName, across the ranks species, variety, subspecies, and form. 

formal_species_records <- db |>
  mutate(
    taxonRank_clean = taxonRank |>
      as.character() |>
      str_squish() |>
      str_to_lower(),
    species_name = scientificName |>
      as.character() |>
      str_squish() |>
      str_to_lower()
  ) |>
  filter(
    taxonRank_clean %in%
      c("species", "variety", "subspecies", "form"),
    !is.na(species_name),
    species_name != ""
  )

formal_species_summary <- formal_species_records |>
  summarise(
    Records = n(),
    Unique_formally_named_species = n_distinct(species_name)
  )

formal_species_by_rank <- formal_species_records |>
  group_by(taxonRank_clean) |>
  summarise(
    Records = n(),
    Distinct_species_names = n_distinct(species_name),
    .groups = "drop"
  ) |>
  arrange(desc(Records))

name_reconciliation_status <- formal_species_records |>
  mutate(
    synonym_status = synonymEpig |>
      as.character() |>
      str_squish() |>
      str_to_lower(),
    synonym_status = replace_na(synonym_status, "missing")
  ) |>
  count(synonym_status, name = "Records") |>
  mutate(
    Percentage = round(Records / sum(Records) * 100, 2)
  ) |>
  arrange(desc(Records))

formal_species_summary
formal_species_by_rank
name_reconciliation_status

# The central formal-taxonomy metric is now established:
#   
# - 1,363,198 formally named occurrence records
# - 15,157 unique formally named species
# - 15,131 appear among records ranked directly as species
# - Infraspecific records add only 26 species not already represented by 
# species-ranked records
# 
# The per-rank distinct counts should not be added together because many names 
# occur under multiple ranks.
# 
# At the record level:
#   
# - 75.8% have accepted reconciliation status
# - 10.3% were reconciled from a synonym
# - 13.8% have missing reconciliation status

library(sf)
library(dplyr)
library(purrr)
library(tibble)

map_dir <- file.path(
  "C:/EpIG_v2.0_",
  "EpIG_volunteership",
  "data",
  "Occurrences_visualization",
  "Neotropics_Morrone"
)

layer_paths <- list.files(
  map_dir,
  pattern = "\\.shp$",
  full.names = TRUE
)

layer_audit <- purrr::map_dfr(layer_paths, \(path) {
  spatial_layer <- sf::st_read(path, quiet = TRUE)
  
  tibble::tibble(
    layer_name = basename(path),
    features = nrow(spatial_layer),
    geometry = paste(
      unique(as.character(sf::st_geometry_type(spatial_layer))),
      collapse = ", "
    ),
    crs = sf::st_crs(spatial_layer)$input,
    valid_features = sum(sf::st_is_valid(spatial_layer), na.rm = TRUE),
    invalid_features = sum(!sf::st_is_valid(spatial_layer), na.rm = TRUE),
    attribute_fields = paste(
      names(sf::st_drop_geometry(spatial_layer)),
      collapse = ", "
    )
  )
})

print(layer_audit, n = Inf, width = Inf)

