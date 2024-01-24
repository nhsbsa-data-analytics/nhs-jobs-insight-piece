# Section Start: Loading packages ----

source(here::here('R/text_processing/load_packages.R'))
                  


# Section 1: Connect to the database and collect data --------------------------

# Create a connection to the database
con = nhsbsaR::con_nhsbsa(database = 'DALP')


# Connect to the table containing pdf and docx files
JD_data = dplyr::tbl(
  src = con,
  from = "INT644_JD_DATA_DP")%>%
  filter(FILE_TYPE %in% c('PDF','DOCX'))




# Join the base data info to the JD_data table
vacancy_references = JD_data %>%
  filter(
    FILE_NAME != 'dummy.docx',
    FILE_NAME != 'VerificationDoc.docx',
    REGEXP_LIKE(FILE_NAME, "*.pdf|*.docx"),
    !REGEXP_LIKE(FILE_NAME, '*Poster*|*Nurse_paramedics_half-page_ad*'),
    FILE_TYPE %in% c('PDF','DOCX'),
    #custom filter for files that dont work
    VAC_REF !="916695193",
    VAC_REF !="916695161"
  ) %>% 
  select(VAC_REF)%>%
  pull()


# Save files to the tempdir
output_dir = tempdir()

# Create above temp dir if not already in place
if (!file.exists(output_dir)) {dir.create(output_dir)}


output_text=list()


for(i in 1:length(vacancy_references)){
  
  vac_ref=vacancy_references[i]
  
    # Pull all binary for vac_refs within chunk
  hex_integer = JD_data %>%
    filter(VAC_REF==vac_ref) %>%
    select(FILE_BINARY) %>%
    pull()
  
  # Pull all file_names for vac_refs within chunk
  file_name = JD_data %>%
    filter(VAC_REF==vac_ref) %>%
    select(FILE_NAME) %>%
    pull()
  
  #check if valid string
  valid_string=validUTF8(file_name)
  
  for(file_idx in 1:length(hex_integer)){
    
    skip_to_next=FALSE
  
  if(!valid_string[file_idx]){
    next
  }
  
  
  if(is.null(hex_integer[[file_idx]])){next}
  
  if(endsWith(tolower(file_name[file_idx]), '.pdf')){
    # File output name and directory
    file_name_path = paste0(output_dir, "/output_", vac_ref, ".pdf")
  }else if (endsWith(tolower(file_name[file_idx]), '.docx')){
    
    file_name_path = paste0(output_dir, "/output_", vac_ref, ".docx" )
  }else(next)
  
  # Write binary
  writeBin(hex_integer[[file_idx]], file_name_path, size = 1)
  
  
  tryCatch({
  if(endsWith(tolower(file_name[file_idx]), '.pdf')){
    
    file_text = tabulizer::extract_text(file_name_path)
    
    #file.remove(file_name_path)
  }else if (endsWith(tolower(file_name[file_idx]), '.docx')){
    
    file_text = readtext::readtext(file_name_path)$text
    
    #file.remove(file_name_path)
  }
    
  },
   error = function(e) { skip_to_next <<- TRUE
    message(paste("There was an error reading the file: ", file_name[file_idx]))
    
  })
  
  if(skip_to_next==TRUE){ next}
  
  
  
  if(endsWith(tolower(file_name[file_idx]), '.pdf')){
    
    file_text = tabulizer::extract_text(file_name_path)
    
    #file.remove(file_name_path)
  }else if (endsWith(tolower(file_name[file_idx]), '.docx')){
    
    file_text = readtext::readtext(file_name_path)$text
    
    #file.remove(file_name_path)
  }
  # Attempt to write each binary to a doc and read back in
   # Read text back in, depending on format
  
      # Output as a dataframe
   output_text[[paste0(vac_ref,file_idx)]]= data.frame(VAC_REF = vac_ref, text = file_text)
   
  }
  
}


jd_text=do.call('rbind',output_text)



# Bind all rows for all output within 'job_description_text' list
job_description_text_tbl = jd_text %>% 
  bind_rows() %>% 
  group_by(VAC_REF) %>%
  summarise(text = paste(text, collapse = ' ')) %>%
  ungroup()



# Save output
saveRDS(job_description_text_tbl, './data/job_description_text_DATA_PROFESSIONAL.rds')


# Disconnect
DBI::dbDisconnect(con)


