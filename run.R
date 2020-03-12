# This script creates a "clean" Ofsted time series dataset to be used consistently for analysis purposes.
# It draws together all published datasets on the MI section of GOV.UK
# The desired primary key combination is urn, publication_date
# This dataset can then be used to work out the Ofsted rating of a school at any date

library(dplyr)
library(readxl)
library(janitor)
library(xml2)
library(rvest)
library(qdapRegex)
library(lubridate)


# Source the gias predecessors code ---------------------------------------
source("R/gias_predecessors.R")
source("R/gias.R")

# Create temp directory ---------------------------------------------------

tmp_dir <- tempdir()
ofsted_dir <- file.path(tmp_dir, "ofsted")
if (dir.exists(ofsted_dir)) unlink(ofsted_dir, recursive = TRUE) 
dir.create(ofsted_dir)

# Get links to all ofsted publications ----------------------------------------------

url <- "https://www.gov.uk/government/statistical-data-sets/monthly-management-information-ofsteds-school-inspections-outcomes"

pg <- read_html(url)

monthly_files <- html_attr(html_nodes(pg, "a"), "href") %>% 
  as_tibble() %>%
  filter(
    (
    # Only excel files
    grepl("https://assets.publishing.service.gov.uk/", value) & grepl(".xlsx", value) &
    # Only files after specific date range
    rm_between(value, "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/", "/", extract = TRUE) >= 522543
    ) |
    value %in% 
      c("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/518183/Management_information_-_Schools_-_31_January_2016.xlsx",
        "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/507161/External_Management_information_-_Schools___Dec-2015.xlsx",
        "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/507165/External_Management_information_-_Schools___Nov_2015.xlsx")
    ) %>%
  distinct() %>%
  pull(value)

historical_file <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/485634/Management_information_-_schools_-_1_Sept_2005_to_31_August_2015.xlsx"

# Download files -------------------------------------------------------

# Create function that downloads file with base file name to data folder
download_w_file_name <- function(url, folder){
  
  download.file(url, mode="wb", destfile = file.path(folder, basename(url)))
  
}

# Download

num_files <- length(monthly_files)

lapply(monthly_files, download_w_file_name, folder = ofsted_dir)

print(paste(num_files, "files downloaded"))

# Get the right tab for data from each file -------------------------------------------------

excel_sheets_long <- function(file){
  data.frame(path = file, file = basename(file), sheet = excel_sheets(file))
}

sheets <- lapply(file.path(ofsted_dir,list.files(ofsted_dir)), excel_sheets_long) %>%
  bind_rows() %>%
  filter(sheet %in% c("School level data", "Most recent inspections", "Latest Inspections",
                      "Most recent Inspections", "Latest inspections" ))

print(paste(nrow(sheets), "files with correct data tab"))

# Create fuctions for reading in the data ---------------------------------------------

read_ofsted <- function(file, sheet = 1){
  
  read_xlsx(file, skip = 1, sheet = sheet, na = c("NA", NA, NULL, "NULL")) %>%
    clean_names()
  
}

# Create intermediate fucntion that will let us loop over rows of sheets df
read_ofsted_i <- function(i){
  
  read_ofsted(sheets$path[i], sheet = sheets$sheet[i])
  
}

## Get the right columns for data from each file  --------------------------------

# Create function that gets column names for all files (excluding columns not of interest)

read_ofsted_colnames <- function(i){
  
  cols <- read_ofsted_i(i) %>%
    colnames()
  
  tibble(file = sheets$file[i], col = cols) %>%
    filter(
      !grepl("previous", col), 
      !grepl("short", col),
      !grepl("section_8", col),
      !(col %in% c("web_link", "school_name",
                   "ofsted_phase", "type_of_education", "admissions_policy",
                   "sixth_form", "faith_denomination", "ofsted_region", "region",
                   "local_authority", "parliamentary_constituency", "postcode",
                   "the_income_deprivation_affecting_children_index_idaci_quintile",
                   "the_quntile_for_income_deprivation_affecting_children_index_idaci",
                   "the_income_deprivation_affecting_children_index_idaci",
                   "total_pupils", "school_open_date", "designated_religious_character",
                   "religious_ethos", "issue_details", "the_income_deprivation_affecting_children_index_idaci_2010_quintile",
                   "does_the_latest_full_inspection_relate_to_the_urn_of_the_current_school",
                   "x47", "issue", "total_number_of_pupils", "x71", "number_of_other_section_8_inspections_since_last_full_inspection",
                   "faith_grouping", "faith_grouping", "event_type_grouping", "inspection_start_date", "inspection_type_grouping",
                   "school_name_at_time_of_latest_full_inspection", "school_type_at_time_of_latest_full_inspection"
                   ,"x_1"))
    )
  
}

cols <- lapply(1:num_files, read_ofsted_colnames) %>%
  bind_rows() %>%
  group_by(col) %>%
  count() %>%
  ungroup() %>%
  arrange(n)

clean_cols <- cols %>%
  mutate(
    clean_cols = case_when(
      col == "inspection_number_of_latest_full_inspection" ~ "inspection_id",
      col %in% c("inspection_end_date", "inspection_date") ~ "inspection_date",
      col %in% c("category_of_concern", "category") ~ "category",
      col %in% c("outcomes_for_pupils","outcomes_for_children_and_learners") ~ "outcomes_for_children_and_learners",
      col %in% c("sixth_form_provision_where_applicable", 
                 "x16_19_study_programmes_where_applicable", 
                 "x16_to_19_study_programmes", 
                 "x16_to_19_study_programmes_where_applicable") 
      ~ "x16_to_19_study_programmes_where_applicable",
      col == "publication_date" ~ "publication_date",
      col %in% c("is_safeguarding_effective","safeguarding_is_effective") ~ "is_safeguarding_effective",
      col == "urn_at_time_of_latest_full_inspection" ~ "urn_at_time_of_latest_full_inspection",
      col == "laestab_at_time_of_latest_full_inspection" ~ "laestab_at_time_of_latest_full_inspection",
      
      col == "personal_development_behaviour_and_welfare" ~ "personal_development_behaviour_and_welfare",
      col == "quality_of_teaching_learning_and_assessment" ~ "quality_of_teaching_learning_and_assessment",
      
      col == "quality_of_education" ~ "quality_of_education",
      col == "personal_development" ~ "personal_development",
      col == "behaviour_and_attitudes" ~ "behaviour_and_attitudes",
      
      n == num_files ~ col,
      TRUE ~ "NA"
    )
  )

# Create function that cleans ofsted column names ------------------------------------------------
clean_ofsted <- function(df){
  
  cols <- colnames(df)[colnames(df) %in% clean_cols$col]
  
  df <- df %>%
    select_at(vars(cols))
  
  colnames(df) <- clean_cols$clean_cols[match(names(df), clean_cols$col)]
  
  # Some of the old files dont have publication dates
  if (!("publication_date" %in% names(df))) {
    df <- df %>%
      mutate(publication_date = inspection_date)

    print("No publication date field, inspection date used")

  }
  
  # Some of the old files dont have outcomes_for_children_and_learners
  if (!("outcomes_for_children_and_learners" %in% names(df))) {
    df <- df %>%
      mutate(outcomes_for_children_and_learners = NA)
    
    print("No outcomes_for_children_and_learners field, NA imputed")
    
  } 
  
  # Some of the old files dont have quality_of_teaching_learning_and_assessment
  if (!("quality_of_teaching_learning_and_assessment" %in% names(df))) {
    df <- df %>%
      mutate(quality_of_teaching_learning_and_assessment = NA)
    
    print("No quality_of_teaching_learning_and_assessment field, NA imputed")
    
  } 
  
  # Some of the old files dont have personal_development_behaviour_and_welfare
  if (!("personal_development_behaviour_and_welfare" %in% names(df))) {
    df <- df %>%
      mutate(personal_development_behaviour_and_welfare = NA)
    
    print("No personal_development_behaviour_and_welfare field, NA imputed")
    
  } 
  
  
  # Some of the old files dont have behaviour_and_attitudes
  if (!("behaviour_and_attitudes" %in% names(df))) {
    df <- df %>%
      mutate(behaviour_and_attitudes = NA)
    
    print("No behaviour_and_attitudes field, NA imputed")
    
  } 
  
  # Some of the old files dont have quality_of_education
  if (!("quality_of_education" %in% names(df))) {
    df <- df %>%
      mutate(quality_of_education = NA)
    
    print("No quality_of_education field, NA imputed")
    
  } 
  
  # Some of the old files dont have personal_development
  if (!("personal_development" %in% names(df))) {
    df <- df %>%
      mutate(personal_development = NA)
    
    print("No personal_development field, NA imputed")
    
  } 
  
  
  # Some of the old files dont have is_safeguarding_effective
  if (!("is_safeguarding_effective" %in% names(df))) {
    df <- df %>%
      mutate(is_safeguarding_effective = NA)
    
    print("No is_safeguarding_effective field, NA imputed")
    
  }

  # Some of the old files dont have urn_at_time_of_latest_full_inspection
  if (!("urn_at_time_of_latest_full_inspection" %in% names(df))) {
    df <- df %>%
      mutate(urn_at_time_of_latest_full_inspection = NA)
    
    print("No urn_at_time_of_latest_full_inspection field, NA imputed")
    
  }
  
  # Some of the old files dont have laestab_at_time_of_latest_full_inspection
  if (!("laestab_at_time_of_latest_full_inspection" %in% names(df))) {
    df <- df %>%
      mutate(laestab_at_time_of_latest_full_inspection = NA)
    
    print("No laestab_at_time_of_latest_full_inspection field, NA imputed")
    
  }
  
  
  
  df
  
}

read_clean_ofsted <- function(i){
  
  read_ofsted_i(i) %>%
    clean_ofsted()
  
}

monthly_dataset <- lapply(1:nrow(sheets), read_clean_ofsted) %>%
  bind_rows()

## Clean data --------------------------------------------------------------

monthly_clean_dataset <- monthly_dataset %>%
  # Filter out rows with no data
  filter(!is.na(inspection_id)) %>%
  mutate(
    # Fix inspection id where ITS missing
    inspection_id = ifelse(!grepl("ITS", inspection_id), paste0("ITS", inspection_id), inspection_id),
    # Some duplicates due to missing full stop
    inspection_type = ifelse(
      inspection_type == "Non Exempt School Inspection following a request from provider for inspection",
      "Non Exempt School Inspection following a request from provider for inspection.", 
      inspection_type),
    inspection_type = ifelse(
      inspection_type == "Exempt School Inspection following a request from provider for inspection",
      "Exempt School Inspection following a request from provider for inspection.", 
      inspection_type),
    # Format date properly
    inspection_date = as.Date(inspection_date, format = "yyyy-mm-dd"),
    publication_date = as.Date(publication_date, format = "yyyy-mm-dd")
  ) %>%
  # Remove all the duplicates from having data accross multiple months
  distinct()

# Historical data ---------------------------------------------------------

## Download data -----------------------------------------------------------
download_w_file_name(historical_file, folder = ofsted_dir)

## Read and clean data -----------------------------------------------------

historical_clean_data <- read_excel(file.path(ofsted_dir,"Management_information_-_schools_-_1_Sept_2005_to_31_August_2015.xlsx"), sheet = "2005-2015 Inspections", skip = 1) %>%
  clean_names() %>%
  transmute(
    urn, 
    laestab,
    inspection_id = paste0("ITS", as.character(inspection_number)),
    inspection_type,
    inspection_date = as.Date(inspection_end_date, format = "yyyy-mm-dd"),
    publication_date = as.Date(first_published_date, format = "yyyy-mm-dd"),
    overall_effectiveness,
    category,
    x16_to_19_study_programmes_where_applicable = sixth_form_provision,
    outcomes_for_children_and_learners = early_years_provision,
    outcomes_for_children_and_learners = how_well_do_pupils_achieve,
    quality_of_teaching_learning_and_assessment = quality_of_teaching,
    personal_development_behaviour_and_welfare = behaviour_and_safety_of_pupils,
    effectiveness_of_leadership_and_management = leadership_and_management,
    is_safeguarding_effective = NA,
    urn_at_time_of_latest_full_inspection = NA,
    laestab_at_time_latest_full_inspection = NA,
    quality_of_education = NA,
    personal_development = NA,
    behaviour_and_attitudes = NA
  )


# Bring together the final dataset ----------------------------------------

# Bind all of the data together and remove dupliates
all_data <- monthly_clean_dataset %>%
  bind_rows(historical_clean_data) %>%
  distinct() %>%
  # Create rank for entries based on urn and inspection id
  group_by(urn, inspection_id) %>% 
  mutate(rnk1 = rank(desc(inspection_date), ties.method = "first")) %>%
  ungroup() %>%
  # Filter out duplicates
  filter(rnk1 == 1) %>%
  # Create rank for entries based on urn and inspection date
  group_by(urn, inspection_date) %>% 
  mutate(rnk2 = rank(desc(publication_date), ties.method = "first")) %>%
  ungroup() %>%
  # Filter out duplicates
  filter(rnk2 == 1) %>%
  # Filter out fresh starts based on differing laestabs
  filter(laestab == laestab_at_time_of_latest_full_inspection | is.na(laestab_at_time_of_latest_full_inspection)) %>%
  # Rename at time fields
  rename(
    urn_at_time_of_inspection = urn_at_time_of_latest_full_inspection,
    laestab_at_time_of_inspection = laestab_at_time_of_latest_full_inspection
  ) %>%
  # Subset to columns of interest
  select(
    urn,laestab,urn_at_time_of_inspection, laestab_at_time_of_inspection, inspection_id,inspection_type,inspection_date,publication_date,
    overall_effectiveness,category,x16_to_19_study_programmes_where_applicable,
    early_years_provision_where_applicable,outcomes_for_children_and_learners,
    quality_of_teaching_learning_and_assessment,personal_development_behaviour_and_welfare,
    effectiveness_of_leadership_and_management,is_safeguarding_effective,
    quality_of_education,
    personal_development,
    behaviour_and_attitudes
  )


# Modify data to map to all successor schools -----------------------------

# Add flag for inspection urn based on earliest closed date
all_data_inspection_urn_flag <- all_data %>% 
  left_join(gias, by = c("urn")) %>%
  group_by(inspection_id) %>%
  mutate(
    inspection_urn_flag = rank(close_date, ties.method = "average")
  ) %>%
  ungroup()

# Mark the inspection urn of all inspections
all_data_inspection_urn_all <- all_data_inspection_urn_flag %>%
  left_join(
    all_data_inspection_urn_flag %>% 
      filter(inspection_urn_flag == 1) %>%
      select(urn, inspection_id),
    by = c("inspection_id")
  ) %>%
  select(
    urn = urn.x,
    inspection_urn = urn.y,
    inspection_id,
    inspection_type,
    inspection_date,
    publication_date,
    overall_effectiveness,
    category,
    x16_to_19_study_programmes_where_applicable,
    early_years_provision_where_applicable,
    outcomes_for_children_and_learners,
    quality_of_teaching_learning_and_assessment,
    personal_development_behaviour_and_welfare,
    effectiveness_of_leadership_and_management,
    is_safeguarding_effective,
    quality_of_education,
    personal_development,
    behaviour_and_attitudes
  )

# Create dataset of just inspections with same urn and inspection urn
all_data_inspection_urn_only <- all_data_inspection_urn_all %>%
  filter(urn == inspection_urn)

# Create dataset of inspections for all successors
all_data_non_inspection_urn_only <- predecessors %>%
  select(URN, LinkURN) %>%
  left_join(all_data_inspection_urn_only, by = c("LinkURN" = "urn")) %>%
  filter(!is.na(inspection_id)) %>% 
  rename(urn = URN) %>%
  select(-LinkURN)

# Bind togehter to make the "calculated dataset"
ofsted_all_calculated_successors <- bind_rows(
  all_data_inspection_urn_only,
  all_data_non_inspection_urn_only
)

# Identify the successors that ofsted mark that arent in our dataset
left_overs <- all_data_inspection_urn_all %>%
  select(urn, inspection_id) %>%
  left_join(ofsted_all_calculated_successors, by = c("urn", "inspection_id")) %>%
  filter(is.na(overall_effectiveness)) %>%
  select(urn, inspection_id) %>% 
  left_join(all_data_inspection_urn_all, by = c("urn", "inspection_id")) %>%
  select(urn, inspection_urn, inspection_id, 4:ncol(.))

all_data_final <- bind_rows(
  ofsted_all_calculated_successors,
  left_overs
)

write.csv(all_data_final, "outputs/ofsted_all.csv", row.names = FALSE, na = "")

