## ============================================================
                   ## CONTINENT project ##              
                ## Summary of Epiphytic Taxa##
     ## Script for Spatial and Taxonomic Exploration##
## ============================================================

## ============================================================
## BEFORE RUNNING THIS SCRIPT

##Before running the analysis:
##    - Download the accompanying dataset:
##      "epiphytes_final.rds"
##
##Set the working directory before running the script.
##    Example:
##    setwd("C:/Users/your_name/project_folder")
##
##The first execution may take several minutes because
##    packages may need to be installed automatically.
##
## Internet access is required the first time the script
##    runs because packages are downloaded from CRAN.
##
##Interactive maps and tables may require additional memory
##    depending on dataset size and computer specifications.

#OBJECTIVES

## By completing this practical, students should be able to:
##
## - Import and manipulate biodiversity datasets,
## - Work with sf spatial objects,
## - Summarise taxonomic richness,
## - Visualise global occurrence patterns,
## - Generate exploratory biodiversity tables
## ============================================================

## Step 1: Install and load required packages
## Install packages only if they are not already available

required_packages <- c(
  "dplyr", "tidyr", "sf", "ggplot2", "plotly", "DT", "rnaturalearth")

# Identify packages that are not currently installed
missing_packages <- required_packages[
  !required_packages %in% installed.packages()[, "Package"]]

# Install missing packages from CRAN
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

## ============================================================
## Load required libraries silently
## suppressPackageStartupMessages() prevents unnecessary
## package loading messages from cluttering the console
## ============================================================
suppressPackageStartupMessages({
  # Data manipulation and tabular workflows
  library(dplyr)
  # Data reshaping and tidying functions
  library(tidyr)
  # Spatial vector data handling and simple features support
  library(sf)
  # Static data visualisation using the grammar of graphics
  library(ggplot2)
  # Interactive visualisation tools for ggplot objects
  library(plotly)
  # Interactive HTML tables
  library(DT)
  # Natural Earth global basemap data
  library(rnaturalearth)
})

## Step 2: Load Occurrence Data
##
## The dataset contains spatial occurrence records for
## all epiphytic plant species.
##
## The object is stored as an RDS file to preserve:
## - taxonomic information,
## - spatial geometry,
## - and metadata structure.
##
## readRDS() imports the object exactly as it was saved.
occ_raw <- readRDS("reports/epiphytes_teaching.rds")

## Step 3: Filter True Epiphytes
## epi_type == "E" retains only species classified as
## true epiphytes.
occ_true <- occ_raw %>%
  filter(epi_type == "E")

# Remove the original object to reduce memory usage
rm(occ_raw)

## Step 4: Remove Unwanted Taxa
## This section removes:
## - Zamiaceae
## - lycophyte families

lycophyte_families <- c(
  "Lycopodiaceae",
  "Selaginellaceae")

occ_true <- occ_true %>%
  filter(
    (is.na(wcvp_family) | wcvp_family != "Zamiaceae") &
      !(wcvp_family %in% lycophyte_families))

## Step 5: Define Major Taxonomic Groups
## Species are classified into:
## - ferns
## - angiosperms

fern_families <- c(
  "Aspleniaceae", "Cyatheaceae", "Dipteridaceae", "Hymenophyllaceae",
  "Hypodematiaceae", "Lindsaeaceae", "Ophioglossaceae", "Polypodiaceae",
  "Psilotaceae", "Pteridaceae", "Schizaeaceae")

occ_true <- occ_true %>%
  mutate(
    group = if_else(
      wcvp_family %in% fern_families,
      "fern",
      "angiosperm"))

# Generate vectors containing unique species names
epi_angios <- unique(
  occ_true$wcvp_name[
    occ_true$group == "angiosperm"])

epi_ferns <- unique(
  occ_true$wcvp_name[
    occ_true$group == "fern"])

## Step 6: Load Global Basemap

## Natural Earth polygons are used to provide geographic
## context for occurrence records.
world <- ne_countries(
  scale = "medium",
  returnclass = "sf") %>%
  filter(continent != "Antarctica")
# Disable spherical geometry calculations for compatibility
sf_use_s2(FALSE)

## Step 7: Create Interpretation Function

## This function assigns interpretation labels to percentage
## ranges in summary tables.
get_interpretation <- function(pct) {
  case_when(
    pct == 100 ~ "100%: Entire group listed as epiphytic",
    pct >= 30 ~ "30–70%: Very high proportion",
    pct >= 15 ~ "15–30%: High proportion",
    pct >= 5 ~ "5–15%: Moderate proportion",
    pct >= 1 ~ "1–5%: Low proportion",
    TRUE ~ "<0.1–1%: Very low proportion")}
## ============================================================
## NOTE ON SPATIAL VISUALISATION
## The maps generated below represent occurrence records and
## not species ranges or abundance estimates.
##
## Point density may reflect:
## - biological diversity,
## - collection effort,
## - taxonomic interest,
## - or sampling accessibility.
## ============================================================

################################################################################
## ANGIOSPERMS ##

## Step 8: Extract Angiosperm Records
angios_data <- occ_true %>%
  filter(group == "angiosperm") %>%
  mutate(
    wcvp_genus = sub(" .*", "", wcvp_name))

## Step 9: Spatial Distribution of Angiosperms
angios_map <- ggplot() +
  geom_sf(data = world, fill = "gray80", colour = "gray80", size = 0.1) +
  geom_sf(data = angios_data, colour = "forestgreen", alpha = 0.1,size = 0.05) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank())

angios_map

## ============================================================
## NOTE ON TAXONOMIC PROPORTIONS
## The following analysis calculates the proportional
## contribution of each genus to total species richness.
## Species counts are based on unique species names.

## Step 10: Species Proportion per Genus (Angiosperms)

## This analysis asks:
## "What percentage of all epiphytic angiosperm species
## belongs to each genus?"
## ============================================================

# Count total unique species within angiosperms
total_angios_sp <- n_distinct(
  angios_data$wcvp_name)

angios_sp_table <- angios_data %>%
  st_drop_geometry() %>%
  group_by(wcvp_genus) %>%
  summarise(
    species_count = n_distinct(wcvp_name),
    .groups = "drop") %>%
  mutate(
    proportion = round(
      (species_count / total_angios_sp) * 100,
      2)) %>%
  mutate(
    interpretation = get_interpretation(proportion)) %>%
  arrange(desc(proportion))

angios_sp_table

## Step 11: Interactive Angiosperm Summary Table
## datatable() creates interactive HTML tables that allow:
## - sorting,
## - searching,
## - and filtering.
## ============================================================
datatable(
  angios_sp_table,
  colnames = c(
    "Genus",
    "Species count",
    "% of family",
    "Range interpretation"),
  caption = "Table 1: Relative richness of species per genus within epiphytic angiosperms.",
  options = list(
    pageLength = 10,
    dom = "ftp"))
################################################################################
## FERNS ##

## Step 12: Extract Fern Records
ferns_data <- occ_true %>%
  filter(group == "fern") %>%
  mutate(
    wcvp_genus = sub(" .*", "", wcvp_name))

## Step 13: Spatial Distribution of Ferns
ferns_map <- ggplot() +
  geom_sf(data = world, fill = "gray80", colour = "gray80", size = 0.1) +
  geom_sf(data = ferns_data, colour = "forestgreen", alpha = 0.1, size = 0.05) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank())

ferns_map

## Step 14: Species Proportion per Genus (Ferns)
## This analysis asks:
## "What percentage of all epiphytic fern species belongs
## to each genus?"

# Count total unique species within ferns
total_ferns_sp <- n_distinct(
  ferns_data$wcvp_name)

ferns_sp_table <- ferns_data %>%
  st_drop_geometry() %>%
  group_by(wcvp_genus) %>%
  summarise(
    species_count = n_distinct(wcvp_name),
    .groups = "drop") %>%
  mutate(
    proportion = round(
      (species_count / total_ferns_sp) * 100, 2)) %>%
  mutate(
    interpretation = get_interpretation(proportion)) %>%
  arrange(desc(proportion))

ferns_sp_table

## Step 15: Interactive Fern Summary Table
datatable(
  ferns_sp_table,
  colnames = c(
    "Genus",
    "Species count",
    "% of family",
    "Range interpretation"),
  caption = "Table 2: Relative richness of species per genus within epiphytic ferns.",
  options = list(
    pageLength = 10,
    dom = "ftp"))

## ============================================================
## TROUBLESHOOTING
##
## Common issues and solutions:
##
## 1. "File not found"
##    -> Ensure the dataset is inside in your folder.
## 2. "Package not available"
##    -> Update R to a newer version.
## 3. Maps do not appear
##    -> Run plotting lines individually.
## 4. Interactive tables do not open
##    -> Use the RStudio Viewer or a web browser.
## 5. Slow performance
##    -> Large spatial datasets may require more RAM.
## ============================================================
