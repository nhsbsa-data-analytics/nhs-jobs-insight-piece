# Section Start: Loading packages and functions ----

source(here::here('R/text_processing/load_packages.R'))


source('./R/text_processing/functions.R')


# Part One: Load lookups and data ----------------------------------------------
load_all_lookups()


# Load data
jd = readRDS("./data/job_description_text_DATA_PROFESSIONAL.rds") %>% 
  filter(
    !text %in% c('', 'ERROR LOADING', 'POSTER'),
    VAC_REF != ""
  ) %>% 
  rename_all(.fun = tolower) %>% 
  mutate(vac_ref = as.integer(vac_ref)) %>% 
  as.data.table() %>% 
  filter(!is.na(vac_ref))

# Part Two: Function for parlapply ---------------------------------------------

# Part Three: Generate output --------------------------------------------------

# Function to get sentence-level metrics

results_sentence=jd%>%
  group_by(vac_ref)%>%
  rename(word =text) %>% 
  # Unnest bigrams, as some skills bigrams
  unnest_tokens(
    word, 
    word, 
    token = 'ngrams',
    n = 2,
    n_min = 1,
    drop = TRUE,
    to_lower = FALSE
  ) %>% 
  mutate(word=ifelse(nchar(word)>1,tolower(word),word))%>%
  # Only retain skills
  inner_join(skill, by = 'word')%>% 
  # Manually exclude 'ai' from word-level counts
  filter(word != "ai") %>% 
  # Distinct skills plus skill counts
  count(word, name = "skills_count") %>% 
  mutate(skills_distinct = n_distinct(word)) %>% 
  ungroup() %>% 
  # Re-join for skill scores
  left_join(skill %>% select(word, score)) %>% 
  # Generate aggregated score per id
  mutate(score = score * skills_count) %>% 
  mutate(score = sum(score)) %>% 
  ungroup() %>% 
  # Paste skills together
  mutate(word = paste0(word, " (", skills_count, ")")) %>% 
  group_by(vac_ref, skills_distinct, score) %>% 
  summarise(word = paste0(stringr::str_to_sentence(word), collapse = ", ")) %>% 
  ungroup()
  





# Part Four: Process output and save -------------------------------------------


# Rejoin text to id, and vac_ref to just joined text field
output_sentence = results_sentence %>% 
  left_join(jd %>% select(vac_ref, text), by = "vac_ref")%>% 
  select(vac_ref, word, skills_distinct, score) %>% 
  mutate(
    # Deal with NAs
    skills_distinct = ifelse(is.na(skills_distinct), 0, skills_distinct),
    score = ifelse(is.na(score), 0, score),
    word = ifelse(is.na(word), "None", word)
  ) %>% 
  # Rename all to upper once processing finished
  rename_all(.funs = toupper)



# Save output
fwrite(output_sentence, "./data/job_description_skills_DATA_PROFESSIONAL.csv")
