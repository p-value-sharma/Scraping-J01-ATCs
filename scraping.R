# packages ####
library(rvest)
library(dplyr)
library(purrr)
library(tidyr)

# extracting components from WHOCC ATC/DDD website ####

# the website was scraped on July 17, 2018
whocc_j01_url <- 'https://www.whocc.no/atc_ddd_index/?code=J01'

sub_urls <- read_html(x = whocc_j01_url) %>% 
  html_nodes(css = 'a') %>% 
  html_attr('href')

j01_sub_urls_raw <- sub_urls[grepl(pattern = 'J01[[:alpha:]]', x = sub_urls)]
j01_sub_urls <- gsub(pattern = '\\./', replacement = '', j01_sub_urls_raw)

base_url <- 'https://www.whocc.no/atc_ddd_index/'

ATC3_urls <- paste0(base_url, j01_sub_urls)

ATC4_sub_urls_raw <- map(ATC3_urls, ~read_html(.x) %>% 
      html_nodes(css = 'a') %>% 
      html_attr('href'))

ATC4_sub_urls <- ATC4_sub_urls_raw %>% 
  map(~.x[grepl(pattern = 'J01[[:upper:]][[:upper:]]', x = .x)] %>% 
      gsub(pattern = '\\./', replacement = '', .x) %>% 
      paste0(base_url, .)) %>% 
  unlist(.)

ATC5_tables <- ATC4_sub_urls %>% 
  map(~read_html(.x) %>% 
        html_nodes(css = 'table') %>% 
        html_table(header = T)) %>%
  # the output from the pipe has sublists, so we get 
  # rid of those
  map(~as.data.frame(.)) %>% 
  # then bind them all together
  bind_rows(.)



# getting ATC3 and ATC4 names ####

ATC4_names_raw <- map(ATC4_sub_urls, ~read_html(.x) %>% 
                        html_nodes(xpath = '//b[(((count(preceding-sibling::*) + 1) = 8) and parent::*)]//a') %>% 
                        html_text()) %>% 
  unlist()

ATC4_names_df <- data.frame(ATC4_Names = ATC4_names_raw,
                            ATC4 = regmatches(ATC4_sub_urls, regexpr('J01[[:upper:]][[:upper:]]', ATC4_sub_urls), invert = FALSE))


ATC3_names_combined <- whocc_j01_url %>% 
  read_html() %>% 
  html_node(xpath = '//p[(((count(preceding-sibling::*) + 1) = 6) and parent::*)]') %>% 
  html_text() %>% 
  strsplit(x = as.character(ATC3_names_df), split = '\n') %>% 
  unlist() 

ATC3_names_df <- data.frame(combined = ATC3_names_combined) %>% 
  mutate(ATC3 = substr(combined, 1, 4),
         ATC3_Name = str_sub(combined, start = 5)) 

# final ATC table ####
ATC5_tables %>% 
  mutate_at(vars(ATC.code, Name), funs(ifelse(test = grepl(pattern = '^$', x = .), 
                                              NA_character_, 
                                              no = .))) %>% 
  fill(ATC.code, Name, .direction = 'down') %>% 
  rename(ATC5 = ATC.code,
         AntibioticINN = Name,
         RouteAdmin = Adm.R) %>% 
  mutate(AntibioticINN = Hmisc::capitalize(AntibioticINN),
         ATC3 = substr(ATC5, start = 1, stop = 4),
         ATC4 = substr(ATC5, start = 1, stop = 5))

