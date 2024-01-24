#' load_all_lookups Function
#'
#' This function loads data from an Excel file containing technical skills and creates a vector of skill words along with their associated scores.
#'
#' @description
#' The `load_all_lookups` function reads an Excel file containing technical skills and metrics, creates a tibble with two columns (skill and score), and assigns the resulting tibble to the global environment.
#'
#' @details
#' The function uses the `openxlsx` package to read an Excel file named "technical_skills.xlsx" located in the "./data/" directory. It extracts the skill names and their corresponding scores to create a tibble with two columns: "word" and "score". Finally, it assigns this tibble to the global environment with the name "skill".
#'
#' @return
#' The function does not explicitly return a value, but it assigns a tibble containing skill words and scores to the global environment with the name "skill".
#'

load_all_lookups = function(){
  
  #load in the skills list and create a vector of these words
  skill = tibble(
    openxlsx::read.xlsx(
      "./data/technical_skills.xlsx"
    )
  ) %>% 
    select(
      word = skill,
      score = metric
    )
  

  # Assign all objects to global env

  assign(x = "skill", value = skill, envir = globalenv())

}

#' jd_sentence Function
#'
#' This function extracts sentence-level metrics from a data frame containing job descriptions based on a specified chunk index.
#'
#' @description
#' The `jd_sentence` function takes a chunk index as input, filters a data frame (`jd_id`) to obtain sentences belonging to the specified chunk, and then extracts sentence-level metrics using the `get_skills_sentence` function. The resulting metrics are returned.
#'
#' @details
#' The function assumes the existence of a data frame named `jd_id` containing information about job descriptions, including columns "id," "chunk," and "text." It filters the data frame based on the provided `chunk_index` and applies the `get_skills_sentence` function to extract sentence-level metrics.
#'
#' @param chunk_index The index of the chunk for which sentence-level metrics should be extracted.
#'
#' @return
#' A data frame containing sentence-level metrics for the specified chunk index.

#' @export
jd_sentence = function(chunk_index){
  
  # Sentence-level metrics
  sentence = jd_id %>% 
    filter(chunk == chunk_index) %>% 
    get_skills_sentence(., id, text)
  
  return(sentence)
}

#' add_chunk_index Function
#'
#' This function adds a chunk index to a data frame based on a specified block size.
#'
#' @description
#' The `add_chunk_index` function calculates the number of chunks required for a given data frame (`data`) based on a specified block size. It then assigns a unique chunk index to each row in the data frame, adding this information as a new column named "chunk." The modified data frame is returned.
#'
#' @details
#' The function calculates the number of chunks (`chunk_num`) needed to accommodate the data frame based on the provided block size. It then generates a sequence of chunk indices (`chunk`) repeating from 1 to `chunk_num`, ensuring that each row in the data frame receives a unique chunk index. The resulting data frame contains an additional "chunk" column.
#'
#' @param data The input data frame to which chunk indices will be added.
#' @param block_size The specified block size used to determine the number of chunks.
#'
#' @return
#' A modified data frame with an added "chunk" column indicating the chunk index for each row.

add_chunk_index = function(data, block_size){
  
  # Derive chunk info
  chunk_num = ceiling(nrow(data) / block_size)
  chunk = rep(1:chunk_num, block_size)
  chunk = chunk[1:nrow(data)]
  
  # Add chunk index to jd with id field
  data = data %>% cbind(chunk)
  
  # Return
  return(data)
}


#' get_skills_sentence Function
#'
#' This function processes sentence-level metrics for a given data frame, identifying and highlighting skills and competencies.
#'
#' @description
#' The `get_skills_sentence` function takes a data frame, extracts text from specified variables, and performs text processing to identify and highlight skills and competencies. The resulting metrics are returned in a processed data frame.
#'
#' @details
#' The function expects a data frame (`data`) with a unique identifier column (`id`) and variables containing text (`vars`). It renames and processes the text variables, tokenizes the text into words and n-grams, matches them with a predefined skill list (`skill`), and joins competencies (`competency`). It then performs additional text cleaning and highlighting before returning the processed data frame.
#'
#' @param data The input data frame containing text information.
#' @param id The unique identifier column in the data frame.
#' @param vars The variable(s) containing text to be processed.
#'
#' @return
#' A processed data frame with highlighted skills and competencies at the sentence level.
#'
#' @examples
#' \dontrun{
#' get_skills_sentence(my_data, "job_id", c("job_description"))
#' }
#'
#' @import dplyr
#' @import tidyr
#' @import stringi
#' @import stringr
#'
#' @keywords internal
#'
#' @export
get_skills_sentence = function(data, id, vars){
  
  # Sentence-level metrics
  data %>% 
    rename(text := {{vars}}) %>% 
    rename_all(.fun = tolower) %>% 
    unnest_tokens(
      text,
      text,
      token = 'regex',
      pattern = '√|•|●|||\\bo\\b|\\.\\s*|[a-z][.][A-Z]',
      drop = TRUE
    ) %>% 
    unnest_tokens(
      word, 
      text, 
      token = 'ngrams',
      n = 2,
      n_min = 1,
      drop = FALSE
    ) %>% 
    inner_join(skill, by = 'word') %>%
    mutate(
      text = gsub('[^[:alnum:] ]', '', text),
      text = trimws(gsub('\\s+', ' ', text))
    ) %>% 
    regex_left_join(
      competency,
      by = c('text' = 'competency'),
      ignore_case = TRUE
    ) %>%
    distinct() %>%
    mutate(
      text = stringi::stri_replace_all_regex(
        tolower(text),
        paste0('\\b', skills_comp_vec, '\\b', sep = ''),
        paste0('<b>', skills_comp_vec, '</b>'),
        vectorize_all = FALSE
      ),
      text = stringr::str_to_sentence(text),
      text = paste0('<p>', text, '</p>')
    )
}