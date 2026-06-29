############################################################
# INSTALL REQUIRED PACKAGES
############################################################

install.packages(c(
  "tidyverse",
  "sf",
  "janitor",
  "readr",
  "dplyr",
  "stringr"
))

############################################################
# LOAD LIBRARIES
############################################################

library(tidyverse)
library(sf)
library(janitor)
library(readr)
library(dplyr)
library(stringr)

############################################################
# READ THE MAIN CHECKLIST CSV ## kalahandi_butterflies_mishra.csv
############################################################

butt_kld <- read.csv(file.choose(), stringsAsFactors = FALSE) 

# Clean column names
names(butt_kld) <- make.names(names(butt_kld))

############################################################
# BASIC SUMMARY
############################################################

# Total subfamilies
total_subfamilies <- butt_kld %>%
  distinct(Subfamily) %>%
  nrow()

# Total genera
total_genera <- butt_kld %>%
  distinct(Genus) %>%
  nrow()

# Total species
total_species <- butt_kld %>%
  distinct(Genus, Species) %>%
  nrow()

cat("Total Subfamilies:", total_subfamilies, "\n")
cat("Total Genera:", total_genera, "\n")
cat("Total Species:", total_species, "\n")

############################################################
# FAMILY-WISE BREAKDOWN
############################################################

family_breakdown <- butt_kld %>%
  group_by(Family) %>%
  summarise(
    Subfamilies = n_distinct(Subfamily),
    Genera = n_distinct(Genus),
    Species = n_distinct(paste(Genus, Species))
  ) %>%
  arrange(desc(Species))

print(family_breakdown)

############################################################
# ## TABLE EXPORT CODE (OBSCURED FOR NOW)
############################################################

## write.csv(family_breakdown,
##           "Familywise_Subfamily_Genus_Species.csv",
##           row.names = FALSE)

############################################################
# HABITAT BREAKDOWN
# Columns assumed: F, FE, OP, UG
# Values assumed: Yes/No
############################################################

habitat_cols <- c("F", "FE", "OP", "UG")

# Overall habitat usage counts
overall_habitat <- sapply(habitat_cols, function(x) {
  sum(tolower(butt_kld[[x]]) == "yes", na.rm = TRUE)
})

overall_habitat

############################################################
# FAMILY-WISE HABITAT BREAKDOWN
############################################################

family_habitat_breakdown <- butt_kld %>%
  group_by(Family) %>%
  summarise(
    F = sum(tolower(F) == "yes", na.rm = TRUE),
    FE = sum(tolower(FE) == "yes", na.rm = TRUE),
    OP = sum(tolower(OP) == "yes", na.rm = TRUE),
    UG = sum(tolower(UG) == "yes", na.rm = TRUE)
  )

print(family_habitat_breakdown)

############################################################
# SPECIES FOUND ONLY IN ONE HABITAT CATEGORY
############################################################

# Helper function
exclusive_habitat <- function(data, habitat) {
  
  other_habitats <- setdiff(habitat_cols, habitat)
  
  data %>%
    filter(
      tolower(.data[[habitat]]) == "yes"
    ) %>%
    filter(
      rowSums(across(all_of(other_habitats),
                     ~tolower(.) == "yes"),
              na.rm = TRUE) == 0
    ) %>%
    group_by(Family) %>%
    summarise(
      Exclusive_Species = n_distinct(paste(Genus, Species)),
      Total_Family_Species = n_distinct(paste(Genus, Species))
    ) %>%
    mutate(
      Percentage = round(
        (Exclusive_Species / Total_Family_Species) * 100, 2
      )
    )
}

# F only
F_only <- exclusive_habitat(butt_kld, "F")

# FE only
FE_only <- exclusive_habitat(butt_kld, "FE")

# OP only
OP_only <- exclusive_habitat(butt_kld, "OP")

# UG only
UG_only <- exclusive_habitat(butt_kld, "UG")

print(F_only)
print(FE_only)
print(OP_only)
print(UG_only)

############################################################
# ALL POSSIBLE HABITAT COMBINATIONS
############################################################

# Create combination column
butt_kld$Habitat_Combination <- apply(
  butt_kld[, habitat_cols],
  1,
  function(x) {
    
    active <- habitat_cols[tolower(x) == "yes"]
    
    if(length(active) == 0) {
      return("None")
    } else {
      return(paste(active, collapse = "_"))
    }
  }
)

# Count combinations overall
habitat_combinations <- butt_kld %>%
  group_by(Habitat_Combination) %>%
  summarise(
    Species_Count = n_distinct(paste(Genus, Species))
  ) %>%
  arrange(desc(Species_Count))

print(habitat_combinations)

############################################################
# FAMILY-WISE HABITAT COMBINATION BREAKDOWN
############################################################

family_combination_breakdown <- butt_kld %>%
  group_by(Family, Habitat_Combination) %>%
  summarise(
    Species_Count = n_distinct(paste(Genus, Species)),
    .groups = "drop"
  ) %>%
  arrange(Family, desc(Species_Count))

print(family_combination_breakdown)

############################################################
# OCCURRENCE CATEGORY BREAKDOWN
############################################################

# Overall occurrence counts
occurrence_overall <- butt_kld %>%
  count(Occurrence)

print(occurrence_overall, n=nrow(family_occurrence))

# -------------------------------
# Species in RN category
# found ONLY in F habitat
# (F = yes, FE = no, OP = no, UG = no)
# -------------------------------

rn_f_only <- butt_kld %>%
  filter(
    Occurrence == "RN",
    F == "yes",
    FE == "no",
    OP == "no",
    UG == "no"
  )

# -------------------------------
# Number of species
# -------------------------------

rn_f_only_count <- rn_f_only %>%
  summarise(n_species = n_distinct(Species))

cat("\nNumber of RN species found ONLY in F habitat:\n")
print(rn_f_only_count)

# -------------------------------
# Species list
# -------------------------------

rn_f_only_species <- rn_f_only %>%
  mutate(full_species = paste(Genus, Species)) %>%
  distinct(full_species) %>%
  arrange(full_species)

cat("\nRN species found ONLY in F habitat:\n")
print(rn_f_only_species)

############################################################
# SCHEDULE II ANALYSIS
############################################################

schedule2_summary <- butt_kld %>%
  group_by(Family) %>%
  summarise(
    Schedule_II = sum(
      str_detect(
        tolower(Protection.Status),
        "schedule ii"
      ),
      na.rm = TRUE
    ),
    Total_Species = n_distinct(paste(Genus, Species))
  ) %>%
  mutate(
    Percentage = round(
      (Schedule_II / Total_Species) * 100,
      2
    )
  )

print(schedule2_summary)





#### Working on the consolidated dataset of all observations
#### between May 2022 and May 2026
# Install required packages
install.packages(c("dplyr", "sf", "readr"))

# Load libraries
library(dplyr)
library(sf)
library(readr)

# -------------------------------
# Read checklist file
# -------------------------------

cat("Choose the checklist CSV file (Kalahandi Butterflies)\n")
butt_kld <- read_csv(file.choose())

# -------------------------------
# Read iNaturalist file
# -------------------------------

cat("Choose the iNaturalist CSV file (Kalahandi_butterflies_inaturalist)\n")
inat <- read_csv(file.choose())

# -------------------------------
# Modify Potanthus records
# -------------------------------

inat <- inat %>%
  mutate(
    taxon_species_name = ifelse(
      taxon_genus_name == "Potanthus",
      "Potanthus sp",
      taxon_species_name
    )
  )

# -------------------------------
# Remove rows where species is empty
# -------------------------------

inat_clean <- inat %>%
  filter(
    !is.na(taxon_species_name),
    taxon_species_name != ""
  )

# -------------------------------
# Create checklist species names
# -------------------------------

checklist_species <- butt_kld %>%
  mutate(full_species = paste(Genus, Species)) %>%
  pull(full_species) %>%
  unique()

inat_species <- unique(inat_clean$taxon_species_name)

# -------------------------------
# Compare species lists
# -------------------------------

matching_species <- intersect(checklist_species, inat_species)

cat("\nNumber of matching species:\n")
length(matching_species)

# Species present in checklist but absent in iNat
missing_in_inat <- setdiff(checklist_species, inat_species)

cat("\nSpecies present in checklist but absent in iNaturalist:\n")
print(sort(missing_in_inat))

# Species present in iNat but absent in checklist
missing_in_checklist <- setdiff(inat_species, checklist_species)

cat("\nSpecies present in iNaturalist but absent in checklist:\n")
print(sort(missing_in_checklist))

# Export CSV
write.csv(
  inat_clean,
  file.choose(new = TRUE),
  row.names = FALSE
)
# -------------------------------
# Read Karlapat shapefile
# -------------------------------

cat("\nChoose the Karlapat shapefile (.shp)\n")
karlapat_shp <- st_read(file.choose())

# -------------------------------
# 1. Species found inside Karlapat
# -------------------------------

# Convert iNaturalist data to sf object
inat_sf <- st_as_sf(
  inat_clean,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
)

# Match CRS
karlapat_shp <- st_transform(karlapat_shp, st_crs(inat_sf))

# Logical vector for points inside Karlapat
inside_karlapat <- st_within(
  inat_sf,
  karlapat_shp,
  sparse = FALSE
)[,1]

# Add column
inat_sf$inside_karlapat <- inside_karlapat

# -------------------------------
# Create INSIDE subset
# -------------------------------

inat_inside <- inat_sf %>%
  filter(inside_karlapat)

# Number of unique species inside Karlapat
inside_species_count <- inat_inside %>%
  summarise(n_species = n_distinct(taxon_species_name))

cat("\nNumber of species inside Karlapat:\n")
print(inside_species_count)

# -------------------------------
# Family-wise breakdown
# -------------------------------

inside_family_breakdown <- inat_inside %>%
  group_by(taxon_family_name) %>%
  summarise(
    species_count = n_distinct(taxon_species_name)
  ) %>%
  arrange(desc(species_count))

cat("\nFamily-wise breakdown of species inside Karlapat:\n")
print(inside_family_breakdown)

# -------------------------------
# 2. Species ONLY inside Karlapat
# -------------------------------

# Create OUTSIDE subset
inat_outside <- inat_sf %>%
  filter(!inside_karlapat)

# Species lists
species_inside <- unique(inat_inside$taxon_species_name)
species_outside <- unique(inat_outside$taxon_species_name)

# Species found ONLY inside
species_only_inside <- setdiff(
  species_inside,
  species_outside
)

species_only_inside <- sort(species_only_inside)

# -------------------------------
# Species found OUTSIDE Karlapat
# but NOT inside Karlapat
# -------------------------------

# Species lists
species_inside <- unique(inat_inside$taxon_species_name)
species_outside <- unique(inat_outside$taxon_species_name)

# Species found ONLY outside Karlapat
species_only_outside <- setdiff(
  species_outside,
  species_inside
)

species_only_outside <- sort(species_only_outside)

# -------------------------------
# Results
# -------------------------------

cat("\nNumber of species found ONLY outside Karlapat:\n")
length(species_only_outside)

cat("\nSpecies found ONLY outside Karlapat:\n")
print(species_only_outside)

# -------------------------------
# Results
# -------------------------------

cat("\nNumber of species found ONLY inside Karlapat:\n")
length(species_only_inside)

cat("\nSpecies found ONLY inside Karlapat:\n")
print(species_only_inside)

# -------------------------------
# Export checklist of species inside Karlapat
# with save dialog box - OPTIONAL
# -------------------------------

# Create checklist dataframe
karlapat_checklist <- data.frame(
  Species = sort(unique(inat_inside$taxon_species_name))
)

# Choose save location and export
write.csv(
  karlapat_checklist,
  file = file.choose(new = TRUE),
  row.names = FALSE
)

cat("\nChecklist exported successfully.\n")

############ OPTIONAL ANALYSIS ##################

##### SEASONALITY OF BUTTERFLY OCCURRENCE
# -------------------------------
# Bar chart of month-wise butterfly
# species richness
# -------------------------------

# Install package if needed
install.packages("ggplot2")

# Load libraries
library(ggplot2)
library(dplyr)

# -------------------------------
# Extract month
# -------------------------------

inat_monthly <- inat_clean %>%
  mutate(
    Month = substr(observed_on, 6, 7)
  )

# -------------------------------
# Count unique species per month
# -------------------------------

monthly_species <- inat_monthly %>%
  group_by(Month) %>%
  summarise(
    species_count = n_distinct(taxon_species_name)
  )

# -------------------------------
# Convert month numbers to names
# -------------------------------

monthly_species$Month <- factor(
  monthly_species$Month,
  levels = sprintf("%02d", 1:12),
  labels = month.name
)

# -------------------------------
# Total species count
# -------------------------------

total_species <- n_distinct(inat_clean$taxon_species_name)

# -------------------------------
# Plot
# -------------------------------

ggplot(monthly_species,
       aes(x = Month, y = species_count)) +
  
  geom_bar(stat = "identity") +
  
  # Labels on top of bars
  geom_text(
    aes(label = species_count),
    vjust = -0.4,
    size = 4
  ) +
  
  # Total species text in top-left
  annotate(
    "text",
    x = 1,
    y = max(monthly_species$species_count),
    label = paste("Total species =", total_species),
    hjust = 0,
    vjust = -1,
    size = 4
  ) +
  
  labs(
    title = "Month-wise Butterfly Species Richness",
    x = "Month",
    y = "Number of Species"
  ) +
  
  theme_minimal() +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Move the "Total species" label slightly lower so it becomes visible

library(dplyr)
library(ggplot2)
library(lubridate)

# Extract month from observed_on column
month_species <- inat_clean %>%
  mutate(
    Month = month(ymd(observed_on)),
    Month_Name = month.name[Month]
  ) %>%
  group_by(Month, Month_Name) %>%
  summarise(
    Species_Count = n_distinct(taxon_species_name),
    .groups = "drop"
  ) %>%
  arrange(Month)

# Total unique species
total_species <- n_distinct(inat_clean$taxon_species_name)

# Plot
ggplot(month_species, aes(x = factor(Month_Name, levels = month.name),
                          y = Species_Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  
  # Add count labels on top of bars
  geom_text(aes(label = Species_Count),
            vjust = -0.4,
            size = 4.5) +
  
  labs(
    title = "Month-wise Butterfly Species Richness",
    x = "Month",
    y = "Number of Species"
  ) +
  
  # Add total species label (top-left, slightly lower)
  annotate(
    "text",
    x = 1,
    y = max(month_species$Species_Count) * 0.93,
    label = paste("Total species =", total_species),
    hjust = 0,
    size = 5,
    fontface = "bold"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  
  ylim(0, max(month_species$Species_Count) * 1.15)


# Relative abundance of each species
# Relative abundance = number of observations of species / total observations

library(dplyr)

# Total observations in cleaned dataset
total_observations <- nrow(inat_clean)

# Calculate relative abundance
relative_abundance <- inat_clean %>%
  group_by(taxon_species_name) %>%
  summarise(
    Observations = n(),
    Relative_Abundance = Observations / total_observations,
    Percentage = (Observations / total_observations) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(Observations))

# View table
print(relative_abundance, n=nrow(relative_abundance))

# Add Relative Abundance (RA) values to original checklist database
# and export as a new CSV

library(dplyr)

# -------------------------------
# STEP 1: Read checklist database
# -------------------------------

checklist <- read.csv(file.choose(), stringsAsFactors = FALSE)

# -------------------------------
# STEP 2: Create species name column
# -------------------------------

checklist <- checklist %>%
  mutate(
    checklist_species = paste(Genus, Species)
  )

# -------------------------------
# STEP 3: Calculate relative abundance
# from cleaned iNaturalist dataset
# -------------------------------

total_observations <- nrow(inat_clean)

relative_abundance <- inat_clean %>%
  group_by(taxon_species_name) %>%
  summarise(
    Observations = n(),
    RA = Observations / total_observations,
    RA_percent = (Observations / total_observations) * 100,
    .groups = "drop"
  )

# -------------------------------
# STEP 4: Match and merge RA values
# -------------------------------

checklist_with_RA <- checklist %>%
  left_join(
    relative_abundance,
    by = c("checklist_species" = "taxon_species_name")
  )

# -------------------------------
# STEP 5: Export new checklist CSV
# -------------------------------

write.csv(
  checklist_with_RA,
  file.choose(new = TRUE),
  row.names = FALSE
)

# -------------------------------
# STEP 6: Quick summary
# -------------------------------

cat("Total checklist species:",
    nrow(checklist_with_RA), "\n")

cat("Species matched with iNaturalist data:",
    sum(!is.na(checklist_with_RA$RA)), "\n")

