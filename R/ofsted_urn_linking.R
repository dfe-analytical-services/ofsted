library(tidyr)
library(dplyr)
library(janitor)
library(tidylog)
library(curl)
library(RODBC)

links <- read.csv(paste0("https://ea-edubase-api-prod.azurewebsites.net/edubase/downloads/public/links_edubasealldata",gsub("-","",Sys.Date()),".csv"), stringsAsFactors = FALSE) %>% 
  clean_names() %>% 
  mutate(urn = as.character(urn),
         link_urn = as.character(link_urn),
         link_established_date = as.Date(link_established_date, "%d-%m-%Y"))


#write.csv(links, paste0("Data/links_edubasealldata",gsub("-","",Sys.Date()),".csv"), row.names = FALSE)

predecessors <- links %>% 
  filter(grepl('Pred', link_type)) %>% 
  # Flip to be in the direction of successors 
  transmute(predecessor_urn = link_urn,
            successor_urn = urn,
            predecessor_link_type = link_type)

# Full join with predecessors to fill in gaps of missing pairs
successors <- links %>% 
  filter(grepl('Succ', link_type)) %>% 
  full_join(predecessors, by = c("urn" = "predecessor_urn")) %>%
  transmute(urn,
            link_urn = case_when(is.na(link_urn) ~ successor_urn, 
                                 TRUE ~ link_urn),
            successor_link_type = link_type,
            predecessor_link_type) %>% 
  distinct(urn, link_urn, .keep_all = TRUE)


gias <- read.csv(paste0("https://ea-edubase-api-prod.azurewebsites.net/edubase/downloads/public/edubasealldata",gsub("-","",Sys.Date()),".csv"))
  gias2 <- gias %>%  
  clean_names() 
 
   gias <- gias2 %>%
  transmute(urn = as.character(urn),
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

#write.csv(gias, paste0("Data/edubasealldata",gsub("-","",Sys.Date()),".csv"), row.names = FALSE)


successors <- successors %>% 
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


successors <- successors %>% 
  filter(link_status != "Proposed to open") 


successors <- successors %>% 
  filter(urn_status != "Open")

successors <- successors %>% 
  mutate(amalgamation = if_else(urn_close_date > (link_open_date + 365), 1, 0)) %>% 
  mutate(amalgamation = coalesce(amalgamation, 0)) %>% # addresses cases with no close/open dates
  filter(amalgamation != 1) %>% 
  select(-amalgamation)

successors <- successors %>% 
  # Count number of links to a single URN. > 1 indicates a split.
  left_join(count(., urn), by = "urn") %>%
  filter(n == 1) %>% 
  select(-n) %>% 
  # Count number of links to a single LinkURN. > 1 indicates a merge or amalgamation
  left_join(count(., link_urn), by = "link_urn") %>%
  filter(n == 1) %>% 
  select(-n)

same_urn <- filter(successors, urn == link_urn)

successors <- successors %>% 
  filter(is.na(successor_link_type) | !grepl('amal|merg|split', successor_link_type, ignore.case = TRUE)) %>% 
  filter(is.na(predecessor_link_type) | !grepl('amal|merg|split', predecessor_link_type, ignore.case = TRUE)) %>% 
  select(-predecessor_link_type,
         -successor_link_type)

successors <- successors %>% 
  filter((is.na(urn_reason_establishment_closed) | urn_reason_establishment_closed != "1") &
           (is.na(link_reason_establishment_opened) | link_reason_establishment_opened != "1")) #amend to opened

gias <- gias %>% 
  distinct(urn, .keep_all = TRUE)

successors <- successors %>% 
  mutate(infant_to_primary = if_else((urn_phase_of_education == "Primary" & urn_statutory_high_age <= 9 & link_statutory_high_age > 9), 1, 0),
         junior_to_primary = if_else((link_phase_of_education == "Primary" & urn_statutory_low_age >= 7 & link_statutory_low_age < 7), 1, 0)) %>% 
  filter((is.na(infant_to_primary) | infant_to_primary != 1) & (is.na(junior_to_primary) | junior_to_primary != 1))

successors <- successors %>% 
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
  select(urn,
         link_urn)

all_urns <- gias %>% 
  filter(establishment_status != "Proposed to open") %>% 
  transmute(urn = as.character(urn),
            current_urn = urn,
            number_of_links = 0)



while(TRUE){
  all_urns <- all_urns %>%
    left_join(successors, by=c("current_urn"="urn"))
  if(all(is.na(all_urns$link_urn))){
    all_urns <- all_urns %>%
      select(-link_urn)
    
    break
  }
  all_urns <- all_urns %>%
    mutate(number_of_links = ifelse(is.na(link_urn), number_of_links, number_of_links+1),
           current_urn = ifelse(is.na(link_urn), current_urn, link_urn)) %>%
    select(-link_urn)
}



#Test to find infinite loop URNs
#all_urns3 <- all_urns2 %>%
#  left_join(successors_all2, by=c("current_urn"="urn")) %>%
#  transmute(current_urn,urn,link_urn1 = link_urn) %>%
 # left_join(successors_all2, by=c("link_urn1"="urn")) %>%
#  transmute(current_urn,urn,link_urn1,link_urn2 = link_urn) %>%
#  left_join(successors_all2, by=c("link_urn2"="urn")) %>%
#  transmute(current_urn,urn,link_urn1,link_urn2,link_urn3 = link_urn) %>%
#  left_join(successors_all2, by=c("link_urn3"="urn")) %>%
#  transmute(current_urn,urn,link_urn1,link_urn2,link_urn3,link_urn4 = link_urn) %>%
#left_join(successors_all2, by=c("link_urn4"="urn")) %>%
#  transmute(current_urn,urn,link_urn1,link_urn2,link_urn3,link_urn4,link_urn5 = link_urn) %>%
#  left_join(successors_all2, by=c("link_urn5"="urn")) %>%
#  transmute(current_urn,urn,link_urn1,link_urn2,link_urn3,link_urn4,link_urn5,link_urn6 = link_urn) %>%
#left_join(successors_all2, by=c("link_urn6"="urn")) %>%
#  transmute(current_urn,urn,link_urn1,link_urn2,link_urn3,link_urn4,link_urn5,link_urn6,link_urn7 = link_urn) %>%
#  left_join(successors_all2, by=c("link_urn7"="urn")) %>%
#  transmute(current_urn,urn,link_urn1,link_urn2,link_urn3,link_urn4,link_urn5,link_urn6,link_urn7,link_urn8 = link_urn) 


ofsted_urn_links <- all_urns %>%
  distinct() %>%
  transmute(urn = as.character(urn),
            current_urn = as.character(current_urn))


predecessor2 <- successors %>%
  mutate(Level = 1) %>%
  select(
    URN = link_urn,
    LinkURN = urn,
    Level
  )


predecessors2 <- predecessor2

rows <- TRUE

while (rows == TRUE) {
  
  predecessor_next_level2 <- predecessors2 %>%
    filter(Level == max(Level, na.rm = TRUE)) %>%
    select(URN, LinkURN, Level) %>%
    left_join(select(predecessor2, URN, LinkURN), by = c("LinkURN" = "URN")) %>%
    filter(!is.na(LinkURN.y)) %>% 
    filter(LinkURN != LinkURN.y) %>% 
    mutate(
      Level = max(Level, na.rm = TRUE) + 1 #,
      #LinkType = "Predecessor"
    ) %>%
    select(-LinkURN) %>% 
    rename(LinkURN = LinkURN.y)
  
  predecessors2 <- bind_rows(predecessors2, predecessor_next_level2)
  
  rows <- nrow(predecessor_next_level2) > 0 
  
}

# Remove any urns duplicated in history
predecessors2 <- predecessors2 %>% 
  group_by(URN, LinkURN) %>%
  filter(Level == min(Level, na.rm = TRUE)) %>%
  ungroup()

write.csv(predecessors2, "outputs/predecessor_all.csv", row.names = FALSE, na = "")
write.csv(ofsted_urn_links, "outputs/ofsted_current_urn_successor_links.csv", row.names = FALSE, na = "")


