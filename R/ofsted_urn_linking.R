library(tidyr)
library(dplyr)
library(janitor)
library(tidylog)

# Data preparation --------------------------------------------------------

# Download the link files from gias and get clean cols required

gias_links_address <- paste0("https://ea-edubase-api-prod.azurewebsites.net/edubase/downloads/public/links_edubasealldata",gsub("-","",Sys.Date()),".csv")

links <- read.csv(url(gias_links_address, method = "libcurl"), stringsAsFactors = FALSE) %>% 
  clean_names() %>% 
  mutate(urn = as.integer(urn),
         link_urn = as.integer(link_urn),
         link_established_date = as.Date(link_established_date, "%d-%m-%Y"))

# Subset to predecessor links
predecessor_link <- links %>% 
  filter(grepl('Pred', link_type)) %>% 
  # Flip to be in the direction of successors 
  transmute(predecessor_urn = link_urn,
            successor_urn = urn,
            predecessor_link_type = link_type)

# Full join with predecessors to fill in gaps of missing pairs
successor_link <- links %>% 
  filter(grepl('Succ', link_type)) %>% 
  full_join(predecessor_link, by = c("urn" = "predecessor_urn")) %>%
  transmute(urn,
            link_urn = case_when(is.na(link_urn) ~ successor_urn, 
                                 TRUE ~ link_urn),
            successor_link_type = link_type,
            predecessor_link_type) %>% 
  distinct(urn, link_urn, .keep_all = TRUE)

# Read in a gias all data cut and select clean cols required

gias_address <- paste0("https://ea-edubase-api-prod.azurewebsites.net/edubase/downloads/public/edubasealldata",gsub("-","",Sys.Date()),".csv")

gias <- read.csv(url(gias_address, method = "libcurl")) %>%
  clean_names() %>%
  transmute(urn = as.integer(urn),
            establishment_name,
            open_date = as.Date(open_date, "%d-%m-%Y"),
            close_date = as.Date(close_date, "%d-%m-%Y"),
            reason_establishment_closed = reason_establishment_closed_code,
            reason_establishment_opened = reason_establishment_opened_code,
            establishment_status = establishment_status_name,
            establishment_type_group = establishment_type_group_name,
            phase_of_education = phase_of_education_name,
            type_of_establishment_name,
            establishment_status_code,
            statutory_low_age,
            statutory_high_age)

# Add extended info from gias onto sucessor link tables
successor_link_extended_info <- successor_link %>% 
  left_join(gias, by = "urn") %>%
  transmute(urn,
            urn_name = establishment_name,
            urn_status = establishment_status,
            urn_reason_establishment_closed = reason_establishment_closed,
            urn_close_date = close_date,
            urn_phase_of_education = phase_of_education,
            urn_statutory_low_age = statutory_low_age,
            urn_statutory_high_age = statutory_high_age,
            link_urn,
            successor_link_type,
            predecessor_link_type) %>%
  left_join(gias, by = c("link_urn" = "urn")) %>% 
  mutate(link_name = establishment_name,
         link_status = establishment_status,
         link_reason_establishment_opened = reason_establishment_opened,
         link_open_date = open_date,
         link_close_date = close_date,
         link_phase_of_education = phase_of_education,
         link_statutory_low_age = statutory_low_age,
         link_statutory_high_age = statutory_high_age)

# Filter successors based on what we want to exclude
successor_link_final <- successor_link_extended_info %>%
  # Filter out urn open and link proposed to open
  filter(
    urn_status != "Open",
    link_status != "Proposed to open"
  ) %>%
  mutate(
    # Create a flag for amalgamations - What does it mean by amalgamations??
    amalgamation = case_when(
      # Explicitly assign when open and close dates are NA to 0 (not an amalgamation)
      is.na(urn_close_date) | is.na(link_open_date) ~ 0,
      # Identify amalgamations based on close date being greater than the link
      # open data + a year.
      urn_close_date > (link_open_date + 365) ~ 1,
      TRUE ~ 0
    )
  ) %>%
  # Filter out amalgamations
  filter(amalgamation != 1) %>% 
  select(-amalgamation) %>%
  # Filter out any cases where link type is amal, merg or split
  filter(is.na(successor_link_type) | !grepl('amal|merg|split', successor_link_type, ignore.case = TRUE)) %>% 
  filter(is.na(predecessor_link_type) | !grepl('amal|merg|split', predecessor_link_type, ignore.case = TRUE)) %>% 
  select(-predecessor_link_type, -successor_link_type) %>%
  # Filter out any cases where reason established is closed is NA or 1 and reason link
  # establishment is opened is NA or 1
  filter((is.na(urn_reason_establishment_closed) | urn_reason_establishment_closed != "1") &
           (is.na(link_reason_establishment_opened) | link_reason_establishment_opened != "1")) %>%
  # Filter out any cases where there are multiple links (i.e. a split)
  group_by(urn) %>%
  mutate(n_links = n()) %>%
  ungroup() %>%
  filter(n_links == 1) %>%
  select(-n_links) %>%
  # Filter out any cases where the link urn relates to multiple schools 
  # (i.e. a merge/amalgamation)
  group_by(link_urn) %>%
  mutate(n_urns = n()) %>%
  ungroup() %>%
  filter(n_urns == 1) %>%
  select(-n_urns) %>%
  # Filter out any cases where the urn and link urn are equal
  filter(urn != link_urn) %>%
  # Filter out cases where an infact school or junior school has changed to primary
  mutate(
    infant_to_primary = if_else((urn_phase_of_education == "Primary" & urn_statutory_high_age <= 9 & link_statutory_high_age > 9), 1, 0),
    junior_to_primary = if_else((link_phase_of_education == "Primary" & urn_statutory_low_age >= 7 & link_statutory_low_age < 7), 1, 0)
  ) %>% 
  filter((is.na(infant_to_primary) | infant_to_primary != 1) & (is.na(junior_to_primary) | junior_to_primary != 1)) %>%
  # Filter out primary merges
  mutate(infant_junior = xor(grepl("infant|first|lower", urn_name, ignore.case = TRUE), 
                             grepl("junior|\\bmiddle\\b|upper", urn_name, ignore.case = TRUE)),
         # flags predecessors that are specifically infant or junior
         infant_junior_specific = ifelse(!grepl("\\bprimary\\b", urn_name, ignore.case = TRUE), "TRUE", "FALSE"), 
         # flags predecessors that don't have the word "primary" in their names
         primary = ifelse(grepl("\\bprimary\\b", link_name, ignore.case = TRUE), "TRUE", "FALSE"), 
         # flags sucessors that have the word "primary" in their names
         primary_specific = ifelse(!grepl("infant|first|junior", link_name, ignore.case = TRUE), "TRUE", "FALSE"), 
         # flags successors that are specifically infant or junior
         primary_merge = if_else(infant_junior == "TRUE" & infant_junior_specific == "TRUE" & primary == "TRUE" & primary_specific == "TRUE", 1, 0)) %>% # creates a flag for junior/infant specific schools that have merged into a primary
  filter(is.na(primary_merge) | primary_merge == 0) %>%
  select(urn, link_urn)

# Create a table of all predecessors --------------------------------------

# Create base dataset
predecessors <- successor_link_final %>%
  mutate(Level = 1) %>%
  # note the switch in urn and link urn
  select(
    URN = link_urn,
    LinkURN = urn,
    Level
  )

# Recursively join predecessors
rows <- TRUE

while (rows == TRUE) {
  
  predecessor_next_level <- predecessors %>%
    filter(Level == max(Level, na.rm = TRUE)) %>%
    select(URN, LinkURN, Level) %>%
    left_join(select(predecessors, URN, LinkURN), by = c("LinkURN" = "URN")) %>%
    filter(!is.na(LinkURN.y)) %>% 
    filter(LinkURN != LinkURN.y) %>% 
    mutate(
      Level = max(Level, na.rm = TRUE) + 1
    ) %>%
    select(-LinkURN) %>% 
    rename(LinkURN = LinkURN.y)
  
  predecessors <- bind_rows(predecessors, predecessor_next_level)
  
  rows <- nrow(predecessor_next_level) > 0 
  
}

# Remove any urns duplicated in history
predecessors <- predecessors %>% 
  group_by(URN, LinkURN) %>%
  filter(Level == min(Level, na.rm = TRUE)) %>%
  ungroup()

# Create table linking all gias urns to their current urn -----------------
# This is not directly used in code but is a useful by-product

# Create base table for looping
current_urn <- gias %>% 
  filter(establishment_status != "Proposed to open") %>% 
  transmute(urn = urn,
            current_urn = urn,
            number_of_links = 0)

# While loop join successors
while (TRUE) {
  current_urn <- current_urn %>%
    left_join(successor_link_final, by = c("current_urn" = "urn"))
  if (all(is.na(current_urn$link_urn))) {
    current_urn <- current_urn %>%
      select(-link_urn)
    break
  }
  current_urn <- current_urn %>%
    mutate(
      number_of_links = ifelse(is.na(link_urn), number_of_links, number_of_links +
                                 1),
      current_urn = ifelse(is.na(link_urn), current_urn, link_urn)
    ) %>%
    select(-link_urn)
}
