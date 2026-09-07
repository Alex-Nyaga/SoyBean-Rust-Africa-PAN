
# PAN-AFRICAN SOYBEAN RUST TRIAL DATASET — STEP 1

# 1. INSTALL & LOAD REQUIRED LIBRARIES


required_packages <- c(
  "tidyverse",   # data wrangling (dplyr, ggplot2, readr, etc.)
  "janitor",     # clean column names
  "sf",          # spatial data handling (for maps)
  "rnaturalearth", # country boundary shapefiles for Africa map
  "rnaturalearthdata",
  "viridis",     # colorblind-friendly palettes
  "scales",      # axis formatting for plots
  "here"         # robust file path handling
)

# Install packages 
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages) > 0) install.packages(new_packages, dependencies = TRUE)

# Load libraries
invisible(lapply(required_packages, library, character.only = TRUE))




# Raw input CSV
input_path  <- "D:/IITA/SBR_Dataset/PAN_SoyBean Rust_New.csv"

# Output root 
output_root <- "D:/IITA/SBR_Dataset/Output_New"

# Create output subfolders
dir.create(file.path(output_root, "01_setup_season_split"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(output_root, "01_setup_season_split", "plots"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(output_root, "01_setup_season_split", "data"), recursive = TRUE, showWarnings = FALSE)

out_plots <- file.path(output_root, "01_setup_season_split", "plots")
out_data  <- file.path(output_root, "01_setup_season_split", "data")



# 3. IMPORT RAW DATA

raw_data <- read_csv(input_path, show_col_types = FALSE) %>%
  clean_names()   # standardizes column names to snake_case (e.g., R6_RUST_Se -> r6_rust_se)

# Quick structural check — printed to console for verification

cat("RAW DATASET STRUCTURE\n")

cat("Rows:", nrow(raw_data), "\n")
cat("Columns:", ncol(raw_data), "\n")
cat("Countries:", n_distinct(raw_data$country), "\n")
cat("Genotypes:", n_distinct(raw_data$gen), "\n")
cat("Locations:", n_distinct(raw_data$loc), "\n")
cat("Years:", paste(sort(unique(raw_data$year)), collapse = ", "), "\n")
cat("Season values:", paste(sort(unique(raw_data$season)), collapse = ", "), "\n")


# Save a structure summary to a text file for record-keeping
sink(file.path(out_data, "raw_data_structure_summary.txt"))
cat("RAW DATASET STRUCTURE SUMMARY\n")
cat("Generated:", as.character(Sys.time()), "\n\n")
cat("Rows:", nrow(raw_data), "\n")
cat("Columns:", ncol(raw_data), "\n")
cat("Countries:", n_distinct(raw_data$country), "\n")
cat("Genotypes:", n_distinct(raw_data$gen), "\n")
cat("Locations:", n_distinct(raw_data$loc), "\n")
cat("Years:", paste(sort(unique(raw_data$year)), collapse = ", "), "\n")
cat("Season values:", paste(sort(unique(raw_data$season)), collapse = ", "), "\n\n")
cat("Column names:\n")
print(names(raw_data))
sink()


# ------------------------------------------------------------------------------
# 4. SPLIT DATASET BY SEASON
#
season1_data <- raw_data %>% filter(season == 1)
season2_data <- raw_data %>% filter(season == 2)

cat("\nSeason 1 rows:", nrow(season1_data), "\n")
cat("Season 2 rows:", nrow(season2_data), "\n")

# Save each season subset as both CSV (portable) and RDS (fast R-native reload)
write_csv(season1_data, file.path(out_data, "season1_data.csv"))
write_csv(season2_data, file.path(out_data, "season2_data.csv"))

saveRDS(season1_data, file.path(out_data, "season1_data.rds"))
saveRDS(season2_data, file.path(out_data, "season2_data.rds"))


# ------------------------------------------------------------------------------
# 5. DIAGNOSTIC PLOT — RECORD COUNT PER SEASON


season_summary <- raw_data %>%
  count(season) %>%
  mutate(season_label = paste("Season", season))

p_season_counts <- ggplot(season_summary, aes(x = season_label, y = n, fill = season_label)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = comma(n)), vjust = -0.5, size = 4.5) +
  scale_fill_viridis_d(option = "D", guide = "none") +
  labs(
    title = "Number of plot records per season",
    subtitle = "Pan-African Soybean Rust Trial Dataset",
    x = NULL,
    y = "Number of records"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(out_plots, "01_records_per_season.png"),
       p_season_counts, width = 7, height = 5, dpi = 300)


# ------------------------------------------------------------------------------
# 6. DIAGNOSTIC PLOT — UNIQUE LOCATIONS PER SEASON


loc_per_season <- raw_data %>%
  group_by(season) %>%
  summarise(unique_locations = n_distinct(loc), .groups = "drop") %>%
  mutate(season_label = paste("Season", season))

p_loc_counts <- ggplot(loc_per_season, aes(x = season_label, y = unique_locations, fill = season_label)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = unique_locations), vjust = -0.5, size = 4.5) +
  scale_fill_viridis_d(option = "D", guide = "none") +
  labs(
    title = "Number of unique trial locations per season",
    x = NULL,
    y = "Unique locations (loc)"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(out_plots, "02_locations_per_season.png"),
       p_loc_counts, width = 7, height = 5, dpi = 300)


# ------------------------------------------------------------------------------
# 7. MAP — TRIAL LOCATIONS COLORED BY SEASON
# ------------------------------------------------------------------------------
# Uses LAT/LON columns to plot every distinct trial site on an Africa base map,
# colored by which season(s) it was used in.

# Get unique site coordinates with season presence
site_coords <- raw_data %>%
  select(loc, country, lat, lon, season) %>%
  distinct() %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  group_by(loc, country, lat, lon) %>%
  summarise(
    season_presence = case_when(
      all(c(1, 2) %in% season) ~ "Both seasons",
      1 %in% season             ~ "Season 1 only",
      2 %in% season             ~ "Season 2 only",
      TRUE                      ~ "Unknown"
    ),
    .groups = "drop"
  )

# Load Africa country boundaries
africa_map <- ne_countries(continent = "Africa", returnclass = "sf")

p_map <- ggplot() +
  geom_sf(data = africa_map, fill = "grey95", color = "grey70", linewidth = 0.2) +
  geom_point(
    data = site_coords,
    aes(x = lon, y = lat, color = season_presence),
    size = 2, alpha = 0.8
  ) +
  scale_color_manual(
    values = c(
      "Season 1 only" = "#2166AC",
      "Season 2 only" = "#B2182B",
      "Both seasons"  = "#1A9850",
      "Unknown"       = "grey50"
    ),
    name = "Season coverage"
  ) +
  coord_sf(xlim = c(-20, 55), ylim = c(-35, 40), expand = FALSE) +
  labs(
    title = "SBR locations by season",
    subtitle = paste0(nrow(site_coords), " unique trial sites"),
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_line(color = "grey90", linewidth = 0.2),
    legend.position = "bottom"
  )

ggsave(file.path(out_plots, "03_trial_locations_map_by_season.png"),
       p_map, width = 9, height = 9, dpi = 300)


# ------------------------------------------------------------------------------
# 8. CONFIRMATION MESSAGE



cat("Season-split data saved to:", out_data, "\n")
cat("Diagnostic plots saved to :", out_plots, "\n")





# ==============================================================================
# PAN-AFRICAN SOYBEAN RUST TRIAL DATASET — STEP 2
# Verify Location-Genotype-Year Consistency (Season 1)
# 
#   - "Location" here = a unique LAT/LON coordinate pair (not the loc code)
#   - A location CAN have multiple DIFFERENT genotypes tested there — that's fine
#   - But if the SAME genotype appears at the SAME location more than once,
#     it must be in DIFFERENT years (repeat testing across seasons/years)
#   - If the same genotype shows up at the same location in the SAME year
#     more than once (outside of normal replication), that's a problem to flag
# ==============================================================================

library(tidyverse)

# ------------------------------------------------------------------------------
# 1. LOAD SEASON 1 DATA 


out_data  <- "D:/IITA/SBR_Dataset/Output_New/01_setup_season_split/data"
step2_out <- "D:/IITA/SBR_Dataset/Output_New/02_location_genotype_year_check"
dir.create(step2_out, recursive = TRUE, showWarnings = FALSE)

season1_data <- readRDS(file.path(out_data, "season1_data.rds"))


# ------------------------------------------------------------------------------
# 2. DEFINE "LOCATION" AS A UNIQUE LAT/LON PAIR

# Round coordinates slightly to avoid floating-point mismatches
# (e.g. -0.983000001 vs -0.983 being treated as different locations)

season1_data <- season1_data %>%
  mutate(
    lat_r = round(lat, 3),
    lon_r = round(lon, 3)
  )


# ------------------------------------------------------------------------------
# 3. FOR EACH (LOCATION + GENOTYPE), LIST THE YEARS IT WAS TESTED


loc_gen_years <- season1_data %>%
  group_by(lat_r, lon_r, gen) %>%
  summarise(
    n_years       = n_distinct(year),
    years_tested  = paste(sort(unique(year)), collapse = ", "),
    n_records     = n(),
    .groups = "drop"
  )


# ------------------------------------------------------------------------------
# 4. FLAG VIOLATIONS

# A violation = same location + same genotype + same year appearing
# in more than one separate trial (env) — i.e. not just normal replication
# within one trial, but the combo showing up as if it were two different trials.

violations <- season1_data %>%
  group_by(lat_r, lon_r, gen, year) %>%
  summarise(n_envs = n_distinct(env), .groups = "drop") %>%
  filter(n_envs > 1)   # same location+genotype+year spread across >1 trial code


# ------------------------------------------------------------------------------
# 5. SIMPLE SUMMARY (printed to console)
# ------------------------------------------------------------------------------

cat("========================================\n")
cat("SEASON 1 — LOCATION x GENOTYPE x YEAR CHECK\n")

cat("Unique locations (lat/lon):", n_distinct(paste(season1_data$lat_r, season1_data$lon_r)), "\n")
cat("Unique location-genotype pairs:", nrow(loc_gen_years), "\n")
cat("Location-genotype pairs tested in >1 year (expected, good):",
    sum(loc_gen_years$n_years > 1), "\n")
cat("Location-genotype pairs tested in only 1 year:",
    sum(loc_gen_years$n_years == 1), "\n")
cat("VIOLATIONS — same location+genotype+year in multiple trial codes:",
    nrow(violations), "\n")
cat("========================================\n")


# ------------------------------------------------------------------------------
# 6. SAVE RESULTS


write_csv(loc_gen_years, file.path(step2_out, "season1_location_genotype_years.csv"))
write_csv(violations,    file.path(step2_out, "season1_violations.csv"))

if (nrow(violations) > 0) {
  cat("\n⚠ Violations found — see season1_violations.csv for details.\n")
} else {
  cat("\n✔ No violations — every same-location/same-genotype repeat happens in a different year.\n")
}




# ==============================================================================
# STEP 3 — COLLAPSE REPLICATES INTO ONE ROW PER (YEAR + LOCATION + SEASON + GEN)
# SEASON 1  |  Guarantees ALL columns are retained 

library(tidyverse)

out_data  <- "D:/IITA/SBR_Dataset/Output_New/01_setup_season_split/data"
step3_out <- "D:/IITA/SBR_Dataset/Output_New/03_collapse_replicates"
dir.create(step3_out, recursive = TRUE, showWarnings = FALSE)

season1_data <- readRDS(file.path(out_data, "season1_data.rds"))

season1_data <- season1_data %>%
  mutate(
    lat_r = round(lat, 3),
    lon_r = round(lon, 3)
  )

# ------------------------------------------------------------------------------
# Custom mode function (NA-safe, "largest value wins" tie-break)

get_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  freq_table  <- table(x)
  max_freq    <- max(freq_table)
  tied_values <- names(freq_table)[freq_table == max_freq]
  if (is.numeric(x)) tied_values <- as.numeric(tied_values)
  max(tied_values)
}

# ------------------------------------------------------------------------------
# Define grouping keys and which columns go to MODE vs MEAN

group_keys <- c("year", "lat_r", "lon_r", "season", "gen")

scale_cols <- c("lod", "pod_shattering",
                names(season1_data)[str_starts(tolower(names(season1_data)), "r6_")])
scale_cols <- intersect(scale_cols, names(season1_data))

categorical_text_cols <- season1_data %>%
  select(-all_of(group_keys)) %>%
  select(where(is.character)) %>%
  names()

mode_cols <- union(scale_cols, categorical_text_cols)

mean_cols <- season1_data %>%
  select(-all_of(group_keys), -rep) %>%
  select(where(is.numeric)) %>%
  names()
mean_cols <- setdiff(mean_cols, mode_cols)

# ------------------------------------------------------------------------------
# SAFETY CHECK — catch ANY column not yet classified 
all_cols      <- names(season1_data)
accounted_for <- unique(c(group_keys, mode_cols, mean_cols, "rep"))
leftover_cols <- setdiff(all_cols, accounted_for)

if (length(leftover_cols) > 0) {
  cat("\n⚠️  WARNING (Season 1) — columns not numeric/character, adding to MODE bucket:\n")
  print(leftover_cols)
  cat("Their classes:\n")
  print(sapply(season1_data[leftover_cols], class))
  mode_cols <- union(mode_cols, leftover_cols)
} else {
  cat("\n✅ Season 1: all columns accounted for — none will be dropped.\n")
}

cat("\nMODE columns (", length(mode_cols), "):\n"); print(mode_cols)
cat("\nMEAN columns (", length(mean_cols), "):\n"); print(mean_cols)

# ------------------------------------------------------------------------------
# FINAL VERIFICATION before collapsing: every single column must be placed

final_check <- setdiff(all_cols, unique(c(group_keys, mode_cols, mean_cols, "rep")))
if (length(final_check) > 0) {
  stop("STOP: these columns are still unaccounted for — fix before proceeding: ",
       paste(final_check, collapse = ", "))
}

# ------------------------------------------------------------------------------
# Collapse

season1_collapsed <- season1_data %>%
  group_by(across(all_of(group_keys))) %>%
  summarise(
    n_reps_combined = n(),
    across(all_of(mode_cols), get_mode),
    across(all_of(mean_cols), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# Save 
write_csv(season1_collapsed, file.path(step3_out, "season1_collapsed_by_replicate.csv"))
saveRDS(season1_collapsed, file.path(step3_out, "season1_collapsed_by_replicate.rds"))

n_before <- nrow(season1_data)
n_after  <- nrow(season1_collapsed)
n_col_before <- ncol(season1_data)
n_col_after  <- ncol(season1_collapsed)  # will be +1 vs original due to n_reps_combined, -1 due to rep dropped

cat("\n========================================\n")
cat("STEP 3 COMPLETE — SEASON 1\n")
cat("========================================\n")
cat("Rows before collapsing   :", n_before, "\n")
cat("Rows after collapsing    :", n_after, "\n")
cat("Rows eliminated (merged) :", n_before - n_after, "\n")
cat("Columns before (incl rep):", n_col_before, "\n")
cat("Columns after (excl rep, +n_reps_combined):", n_col_after, "\n")
cat("========================================\n")











# SEASON 2  |  Guarantees ALL columns are retained (mode, mean, or safety net)
# ==============================================================================

library(tidyverse)

out_data  <- "D:/IITA/SBR_Dataset/Output_New/01_setup_season_split/data"
step3_out <- "D:/IITA/SBR_Dataset/Output_New/03_collapse_replicates"
dir.create(step3_out, recursive = TRUE, showWarnings = FALSE)

season2_data <- readRDS(file.path(out_data, "season2_data.rds"))

season2_data <- season2_data %>%
  mutate(
    lat_r = round(lat, 3),
    lon_r = round(lon, 3)
  )

# ------------------------------------------------------------------------------
# Custom mode function 
get_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  freq_table  <- table(x)
  max_freq    <- max(freq_table)
  tied_values <- names(freq_table)[freq_table == max_freq]
  if (is.numeric(x)) tied_values <- as.numeric(tied_values)
  max(tied_values)
}

# ------------------------------------------------------------------------------
# Define grouping keys and which columns go to MODE vs MEAN

group_keys <- c("year", "lat_r", "lon_r", "season", "gen")

scale_cols <- c("lod", "pod_shattering",
                names(season2_data)[str_starts(tolower(names(season2_data)), "r6_")])
scale_cols <- intersect(scale_cols, names(season2_data))

categorical_text_cols <- season2_data %>%
  select(-all_of(group_keys)) %>%
  select(where(is.character)) %>%
  names()

mode_cols <- union(scale_cols, categorical_text_cols)

mean_cols <- season2_data %>%
  select(-all_of(group_keys), -rep) %>%
  select(where(is.numeric)) %>%
  names()
mean_cols <- setdiff(mean_cols, mode_cols)

# ------------------------------------------------------------------------------
# SAFETY CHECK - catch ANY column not yet classified 
all_cols      <- names(season2_data)
accounted_for <- unique(c(group_keys, mode_cols, mean_cols, "rep"))
leftover_cols <- setdiff(all_cols, accounted_for)

if (length(leftover_cols) > 0) {
  cat("\n⚠️  WARNING (Season 2) — columns not numeric/character, adding to MODE bucket:\n")
  print(leftover_cols)
  cat("Their classes:\n")
  print(sapply(season2_data[leftover_cols], class))
  mode_cols <- union(mode_cols, leftover_cols)
} else {
  cat("\n✅ Season 2: all columns accounted for — none will be dropped.\n")
}

cat("\nMODE columns (", length(mode_cols), "):\n"); print(mode_cols)
cat("\nMEAN columns (", length(mean_cols), "):\n"); print(mean_cols)

# ------------------------------------------------------------------------------
# FINAL VERIFICATION before collapsing: every single column must be placed

final_check <- setdiff(all_cols, unique(c(group_keys, mode_cols, mean_cols, "rep")))
if (length(final_check) > 0) {
  stop("STOP: these columns are still unaccounted for — fix before proceeding: ",
       paste(final_check, collapse = ", "))
}

# ------------------------------------------------------------------------------
# Collapse

season2_collapsed <- season2_data %>%
  group_by(across(all_of(group_keys))) %>%
  summarise(
    n_reps_combined = n(),
    across(all_of(mode_cols), get_mode),
    across(all_of(mean_cols), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# Save 
write_csv(season2_collapsed, file.path(step3_out, "season2_collapsed_by_replicate.csv"))
saveRDS(season2_collapsed, file.path(step3_out, "season2_collapsed_by_replicate.rds"))

n_before <- nrow(season2_data)
n_after  <- nrow(season2_collapsed)
n_col_before <- ncol(season2_data)
n_col_after  <- ncol(season2_collapsed)

cat("\n========================================\n")
cat("STEP 3 COMPLETE — SEASON 2\n")

cat("Rows before collapsing   :", n_before, "\n")
cat("Rows after collapsing    :", n_after, "\n")
cat("Rows eliminated (merged) :", n_before - n_after, "\n")
cat("Columns before (incl rep):", n_col_before, "\n")
cat("Columns after (excl rep, +n_reps_combined):", n_col_after, "\n")
cat("========================================\n")




# ==============================================================================
# STEP 3b - VERIFY COLUMN INTEGRITY
# Compare original season1_data / season2_data vs their collapsed versions
# ==============================================================================

library(tidyverse)

out_data <- "D:/IITA/SBR_Dataset/Output_New/01_setup_season_split/data"
step3_out <- "D:/IITA/SBR_Dataset/Output_New/03_collapse_replicates"

# ------------------------------------------------------------------------------
# Load 
season1_data <- readRDS(file.path(out_data, "season1_data.rds"))
season2_data <- readRDS(file.path(out_data, "season2_data.rds"))
season1_collapsed <- readRDS(file.path(step3_out, "season1_collapsed_by_replicate.rds"))
season2_collapsed <- readRDS(file.path(step3_out, "season2_collapsed_by_replicate.rds"))

# ------------------------------------------------------------------------------
# Helper function: compares an original vs collapsed data set
# ------------------------------------------------------------------------------
compare_cols <- function(original, collapsed, label) {
  orig_cols <- names(original)
  coll_cols <- names(collapsed)
  
  dropped_from_orig <- setdiff(orig_cols, coll_cols)
  added_in_collapsed <- setdiff(coll_cols, orig_cols)
  
  cat("\n========================================\n")
  cat(label, "\n")
  cat("========================================\n")
  cat("Original  columns:", length(orig_cols), "\n")
  cat("Collapsed columns:", length(coll_cols), "\n")
  
  cat("\n-- Columns DROPPED (present in original, missing in collapsed) --\n")
  if (length(dropped_from_orig) == 0) {
    cat("(none)\n")
  } else {
    print(dropped_from_orig)
  }
  
  cat("\n-- Columns ADDED (new in collapsed, not in original) --\n")
  if (length(added_in_collapsed) == 0) {
    cat("(none)\n")
  } else {
    print(added_in_collapsed)
  }
  
  expected_dropped <- c("rep")
  expected_added <- c("n_reps_combined", "lat_r", "lon_r")
  
  unexpected_dropped <- setdiff(dropped_from_orig, expected_dropped)
  unexpected_added <- setdiff(added_in_collapsed, expected_added)
  
  if (length(unexpected_dropped) > 0) {
    cat("\nWARNING - UNEXPECTED DROPPED COLUMNS (real data columns were lost!):\n")
    print(unexpected_dropped)
  }
  if (length(unexpected_added) > 0) {
    cat("\nNOTE - UNEXPECTED ADDED COLUMNS (double-check these are intentional):\n")
    print(unexpected_added)
  }
  if (length(unexpected_dropped) == 0 && length(unexpected_added) == 0) {
    cat("\nOK - Column changes match expected pattern exactly.\n")
  }
}

# ------------------------------------------------------------------------------
# Run comparisons
# ------------------------------------------------------------------------------
compare_cols(season1_data, season1_collapsed, "SEASON 1: original vs collapsed")
compare_cols(season2_data, season2_collapsed, "SEASON 2: original vs collapsed")

# ------------------------------------------------------------------------------
# BONUS: check that season1_collapsed and season2_collapsed match each other
# ------------------------------------------------------------------------------
cat("\n========================================\n")
cat("SEASON 1 vs SEASON 2 collapsed: same columns?\n")
cat("========================================\n")

s1_cols <- names(season1_collapsed)
s2_cols <- names(season2_collapsed)

only_in_s1 <- setdiff(s1_cols, s2_cols)
only_in_s2 <- setdiff(s2_cols, s1_cols)

if (length(only_in_s1) == 0 && length(only_in_s2) == 0) {
  cat("OK - Identical column sets, safe to merge/bind later.\n")
} else {
  cat("MISMATCH - these differ between season 1 and season 2 collapsed datasets:\n")
  cat("Only in season1_collapsed:\n"); print(only_in_s1)
  cat("Only in season2_collapsed:\n"); print(only_in_s2)
}



compare_cols(season1_data, season1_collapsed, "SEASON 1: original vs collapsed")
compare_cols(season2_data, season2_collapsed, "SEASON 2: original vs collapsed")




library(tidyverse)

step3_out <- "D:/IITA/SBR_Dataset/Output_New/03_collapse_replicates"
step4_out <- "D:/IITA/SBR_Dataset/Output_New/04_merged_dataset"

dir.create(step4_out, recursive = TRUE, showWarnings = FALSE)

season1_collapsed <- readRDS(
  file.path(step3_out, "season1_collapsed_by_replicate.rds")
)

season2_collapsed <- readRDS(
  file.path(step3_out, "season2_collapsed_by_replicate.rds")
)

merged_data <- bind_rows(
  season1_collapsed,
  season2_collapsed
)

# Find country column
country_matches <- which(
  tolower(names(merged_data)) == "country"
)

# Find rust severity column
rust_matches <- which(
  tolower(names(merged_data)) == "r6_rust_se"
)

if (length(country_matches) == 0) {
  stop("No column named 'country' was found.")
}

if (length(rust_matches) == 0) {
  stop("No column named 'r6_rust_se' was found.")
}

country_col <- names(merged_data)[country_matches[1]]
rust_sev_col <- names(merged_data)[rust_matches[1]]

# Columns to move to the beginning
lead_cols <- c(
  country_col,
  "lat_r",
  "lon_r",
  "year",
  "gen",
  rust_sev_col
)

# Keep only columns that actually exist
lead_cols <- lead_cols[lead_cols %in% names(merged_data)]

# Reorder columns
merged_data <- merged_data %>%
  relocate(
    all_of(lead_cols),
    .before = everything()
  )

# Sort from oldest year to latest year
if ("year" %in% names(merged_data)) {
  
  merged_data <- merged_data %>%
    mutate(year = as.numeric(year)) %>%
    arrange(year)
  
} else {
  
  warning("The 'year' column was not found, so rows were not sorted by year.")
}

# Save CSV
write_csv(
  merged_data,
  file.path(step4_out, "merged_season1_season2.csv")
)

# Save RDS
saveRDS(
  merged_data,
  file.path(step4_out, "merged_season1_season2.rds")
)

cat("\n========================================\n")
cat("STEP 4 COMPLETE — MERGED DATASET\n")
cat("========================================\n")

cat("Total rows   :", nrow(merged_data), "\n")
cat("Total columns:", ncol(merged_data), "\n")

if ("year" %in% names(merged_data)) {
  cat(
    "Year range   :",
    min(merged_data$year, na.rm = TRUE),
    "-",
    max(merged_data$year, na.rm = TRUE),
    "\n"
  )
}

cat("\nColumn order (first 10):\n")
print(head(names(merged_data), 10))

cat("\nOutput files:\n")
cat(
  file.path(step4_out, "merged_season1_season2.csv"),
  "\n"
)

cat(
  file.path(step4_out, "merged_season1_season2.rds"),
  "\n"
)

cat("========================================\n")

