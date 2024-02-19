# Section Start: Data loading ----

# Connect to the database

con = nhsbsaR::con_nhsbsa(database = "DALP")

# Connect to the table containing the base data

base_data_2122 = dplyr::tbl(
  src = con,
  from = "INT644_BASE_DATA_2122") %>%
  select(
    VAC_REF,
    CLEANED_JOB_TITLE,
    TECH_FLAG,
    VACANCY_AGENCY_NAME,
    HARD_ROLE_FLAG,
    BAND_CD
  ) %>%
  collect()

base_data_2223 = dplyr::tbl(
  src = con,
  from = "INT644_BASE_DATA_2223") %>%
  select(
    VAC_REF,
    CLEANED_JOB_TITLE,
    TECH_FLAG,
    VACANCY_AGENCY_NAME,
    HARD_ROLE_FLAG,
    BAND_CD
  ) %>%
  collect()


#load in the skills extracted for each vac_ref

skills_table = read.table(
  here::here("./data/job_description_skills_DATA_PROFESSIONAL.csv"),
  sep = ',',
  header = TRUE,
  colClasses = c("VAC_REF" = "character")
)


#load in a data table containing the list of technical skills and their grouping
technical_skills_tbl = readxl::read_excel(here::here('./data/technical_skills.xlsx'))

# Section End: Data loading ----

# Section Start: Validate Data ----


# Check the quality before continuing processing.
base_data_2122%<>%
  verify(length(unique(VAC_REF))==nrow(.))%>%#verify unique vac refs
  verify(has_class("VAC_REF",class='character'))%>%#check class of columns is correct
  verify(has_class("CLEANED_JOB_TITLE",class='character'))%>%
  verify(has_class("VACANCY_AGENCY_NAME",class='character'))


  base_data_2223%<>%
  verify(length(unique(VAC_REF))==nrow(.))%>%#verify unique vac refs
  verify(has_class("VAC_REF",class='character'))%>%#check class of columns is correct
  verify(has_class("CLEANED_JOB_TITLE",class='character'))%>%
  verify(has_class("VACANCY_AGENCY_NAME",class='character'))
  
  
skills_table%<>%
  verify(has_all_names("VAC_REF","WORD"))%>%
  verify(has_class("VAC_REF","WORD",class='character'))

  
# Section End: Validate Data ----

# Section Start: Basic table cleaning ----

#merge the base data tables together
base_data=base_data_2122%>%
  union_all(base_data_2223)

rm('base_data_2122','base_data_2223')


#clean up job titles to remove any band information from the name
base_data%<>%
  mutate(CLEANED_JOB_TITLE=stri_replace_all_regex(CLEANED_JOB_TITLE,
                                                  pattern=c('BAND 1','BAND 2','BAND 3','BAND 4',
                                                            'BAND 5','BAND 6','BAND 7','BAND 8',
                                                            'BAND 8A','BAND 8B','BAND 8C','BAND 8D','BAND 9'),
                                                  replacement='',
                                                  vectorize=FALSE),
         CLEANED_JOB_TITLE=str_trim(CLEANED_JOB_TITLE))




#create a skills data table
skills_dt = data.table::data.table(skills_table)

#split the identified skills into a row each and clean
skills = skills_dt[, c(WORD = strsplit(WORD, ",")), by = VAC_REF] %>%
  mutate(WORD = trimws(gsub('\\(.*', '', WORD)),
         WORD = case_when(WORD == 'None' ~ NA,
                          TRUE ~ WORD)) %>%
  left_join(base_data, by = 'VAC_REF')



skills%<>%
  filter(TECH_FLAG==1 | grepl('PERFORMANCE ANALYST',CLEANED_JOB_TITLE)) #filter only those vacancies that are considered to be technical




#clean up the skills table by concatenating similar skills together
skills %<>%
  mutate(WORD = tolower(WORD)) %>%
  left_join(technical_skills_tbl %>%
              select(skill, concatenated_skills),
            by = c('WORD' = 'skill')) %>%
  select(-WORD) %>% #remove original word column
  rename('WORD' = "concatenated_skills") %>% #rename the new column
  distinct() #concatenated skills can return duplicate entries so make sure to
#return distinct rows



rm('skills_table', 'skills_dt', 'technical_skills_tbl')

# Section End: Data loading ----


# Disconnect
DBI::dbDisconnect(con)
