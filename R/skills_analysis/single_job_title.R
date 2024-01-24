#Section Start: Single Vacancy Audit ----

#get core competencies for a data scientist



single_vacancy_identified_skills=get_single_vacancy_skills(skills,VACANCY,'CLEANED_JOB_TITLE',"VACANCY_AGENCY_NAME",'WORD','VAC_REF')



organisation_skill_similarity=compute_skill_similarity(single_vacancy_identified_skills,'all_skills')


#create a square and symmetric similarity matrix (JOB_TITLE X JOB_TITLE)



organisation_skill_similarity %<>%
  mutate(
    VACANCY_NAME_reference = paste0(
      VACANCY_AGENCY_NAME_reference,
      '\n',
      str_to_title(CLEANED_JOB_TITLE_reference),
      '\n',
      sep = ' '
    ),
    VACANCY_NAME = paste0(
      VACANCY_AGENCY_NAME,
      '\n',
      str_to_title(CLEANED_JOB_TITLE),
      '\n',
      sep = ' '
    )
  )


sinlge_title_similarity_matrix=create_similarity_matrix(organisation_skill_similarity,'VAC_REF','similarity')


#Remove NA from the dataframe and replace with 1


sinlge_title_similarity_matrix[is.nan(sinlge_title_similarity_matrix)] = 1


similarity_results_single_vac=group_similar_jobs(sinlge_title_similarity_matrix,single_vacancy_identified_skills,base_data,n_clust,single_vacancy =TRUE)


