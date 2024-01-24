#' Compute how often a skill occurs for each vacancy for a given job title
#'
#' Loads in skill data and computes(for a given job title) all of the skills for each vacancy.
#' Compute how often a skill occurs for each vacancy for a given job title
#'
#' Loads in skill data and computes (for a given job title) all of the skills for each vacancy.
#'
#' @param df A data.frame object.
#' @param vacancy_name Name of the job title of interest.
#' @param jobTitle The column containing the job title information.
#' @param groupingVariable The column that groups the data together. In this instance, it is the organisation name.
#' @param skills The column that contains the skills for each vacancy.
#' @param uniqueIdentifier The column that identifies individual job postings. In this instance, it is the vac ref.
#'
#' @return Returns a local data frame.
#'
#' @export
get_single_vacancy_skills=function(df,vacancy_name,jobTitle,groupingVariable,skills,uniqueIdentifier){
  
  identified_skills = df %>%
    ungroup() %>%
    filter(grepl(paste0(vacancy_name,collapse='|'), !!sym(jobTitle)) & !is.na(!!sym(skills))) %>%
    filter(!grepl('ADMINISTRATOR',CLEANED_JOB_TITLE))%>%#remove roles that contain the word administrator
    group_by(!!sym(groupingVariable)) %>%
    mutate(n_vacancies = n_distinct(!!sym(uniqueIdentifier))) 
  
  
  identified_skills%<>%
    group_by(!!sym(uniqueIdentifier),!!sym(groupingVariable), !!sym(jobTitle), !!sym(skills), n_vacancies) %>%
    summarise(word_frequency = n()) %>% #compute frequency word appears in JD text for a job title in each org
    mutate(proportion_of_vacs = 100 * word_frequency / n_vacancies)%>% #compute as a proportion
    group_by(!!sym(uniqueIdentifier),!!sym(groupingVariable), !!sym(jobTitle)) %>% #group by vacancy and job title
    select(!!sym(groupingVariable), !!sym(jobTitle), !!sym(skills)) %>%
    distinct() %>%
    mutate(WORD=ifelse(str_length(str_trim(!!sym(skills)))<=3,str_to_upper(!!sym(skills)),str_to_title(!!sym(skills))))%>%
    #reformat the skill to all caps if less than 3 characters
    reframe(all_skills = paste0(WORD, collapse = ',')) #group skills together
  
  
  identified_skills%<>%
    rowwise()%>%
    mutate(#unique_skills=paste0(setdiff(str_split(all_skills,',')[[1]],
          #                              commonly_requested_skills),collapse = ','),
           n_skills = str_count(all_skills, ',') + 1)
  
  
  return(identified_skills)  
}



#' Compute how similar two different jobs are.
#'
#' Loads in common skills table and computes how similar the skill sets are between pairs of jobs. Skills are compared using the Jaccard Similarity.
#'
#' @param df A data.frame object.
#' @param skills The column that contains the string of skills separated by commas.
#' 
#' @return Returns a local data frame.
#'
#' @export
compute_skill_similarity=function(df,skills){
  
  if (!('data.frame' %in% class(df))) {
    stop(sprintf("Chosen object is not a dataframe."))
  }
  
  if (!(skills %in% colnames(df))) {
    stop(
      sprintf(
        "The chosen column %s does not exist in the dataframe",
        skills
      )
    )
  }
  
  
  #Test if the skills column if the right type
  
  df%>%
    verify(has_class(skills,class='character'))
  
  
  df %<>%
    cross_join(df,
               suffix = c('_reference', ''))%>% #create a long format table comparing every row pair
    rowwise() %>%
    mutate(overlapping_skills=suppressWarnings(length(intersect(
      strsplit(!!sym(paste0(skills,'_reference')), ',')[[1]], strsplit(!!sym(skills), ',')[[1]]))),
      total_unique_skills=suppressWarnings(length(union(
        strsplit(!!sym(paste0(skills,'_reference')), ',')[[1]], strsplit(!!sym(skills), ',')[[1]]))),
      
      similarity = overlapping_skills / total_unique_skills
    )#compute similarity metric as the overlap of skills relative to the union
  
  return(df)
  
}


#' Create similarity matrix
#'
#' Transforms the output of compute_skill_similarity to create a square similarity matrix. 
#'
#' @param df A data.frame object.
#' @param groupingVariable The column that corresponds to the job title.
#' @param similarity The column corresponding to the similarity of the two jobs.
#' 
#' @return Returns a local data frame which is square.
#'
#' @export
create_similarity_matrix=function(df,groupingVariable,similarity){
  
  
  
  similarity_matrix=df%>%
    select(paste0(groupingVariable,'_reference'),!!sym(groupingVariable),!!sym(similarity))%>%
    pivot_wider(names_from=paste0(groupingVariable,'_reference'),values_from=similarity) 
  
  #remove the first column 
  similarity_matrix <- similarity_matrix[, -1]
  
  
  return(similarity_matrix)
  
}

#' Group jobs together based on skill sets.
#'
#' Takes the similarity matrix, computes MDS and kmeans to group similar jobs together.
#'
#' @param df A data.frame object which is square.
#' @param skills_df A base data frame.
#' @param base_df A boolean checking if we are looking at all of the vacancies or just one.
#' @param n_clust Number of clusters.
#'
#' @return Returns a local data frame.
#'
#' @export
group_similar_jobs=function(df,skills_df,base_df,n_clust,single_vacancy=TRUE){
  
  if((ncol(df)!=nrow(df))){
    stop('The matrix needs to be square')
  }
  if(!(min(df)>=0 & max(df)<=1)){
    stop('The values in the matrix should be bound between 0 and 1')
  }
  
  if(!('CLEANED_JOB_TITLE' %in% colnames(base_df))){
    stop("Base data doesn't contain the column 'CLEANED_JOB_TITLE'")
    
  }
  
  
  mds_fit=cmdscale(1-df,eig=TRUE,k=2,add=TRUE)
  
  set.seed(2023)
  #use the mds embedding and cluster the data using kmeans
  cluster=kmeans((mds_fit$points),n_clust,iter.max = 1000,nstart = 100)$cluster
  
 
    similarity_results=data.frame(
      VAC_REF= skills_df$VAC_REF,
      CLEANED_JOB_TITLE = skills_df$CLEANED_JOB_TITLE,
      VACANCY_AGENCY_NAME = skills_df$VACANCY_AGENCY_NAME,
      Dimension_1 = mds_fit$points[, 1],
      Dimension_2 = mds_fit$points[, 2],
      Cluster = as.factor(cluster)
    ) %>%
      mutate(Type = case_when(
        grepl('SENIOR|LEAD', VACANCY_AGENCY_NAME) ~ 'Senior',
        TRUE ~ 'Regular'
      ))%>%
      left_join(base_df%>%
                  select(VAC_REF,BAND_CD),by='VAC_REF')%>%
      rename('NHS Pay Band'='BAND_CD')
    
  
  
  
  
  return(similarity_results)  
}

#' Check if a data.frame contains any NaN values.
#'
#' This function takes a data.frame as input and checks if it contains any NaN values.
#'
#' @param x A data.frame.
#'
#' @return Returns a data.frame of logical values.
#'
#' @export
is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))


#' Create plot data for an interactive scatter plot.
#'
#' This function takes a data.frame of vacancy skills, and an optional jitter parameter
#' to create plot data suitable for an interactive scatter plot. 
#'
#' @param df A data.frame object.
#' @param vacancy_skills A data.frame of vacancy skills.
#' @param jitter Numeric parameter controlling the amount of jitter to be added (default is 0.04).
#'
#' @return Returns a modified data.frame for creating an interactive scatter plot.
#'
#' @export
create_plot_data=function(df,vacancy_skills,jitter=0.04){
  
  
  plot_data=df%>%
    mutate(Dimension_1=round(Dimension_1,5),
           Dimension_2=round(Dimension_2,5),
           CLEANED_JOB_TITLE=str_to_title(CLEANED_JOB_TITLE))%>%
    group_by(Dimension_1,Dimension_2)%>% #group jobs with same coordinates for tooltip
    mutate(names=paste0(c(CLEANED_JOB_TITLE),collapse='<br>'))%>%
    mutate(CLEANED_JOB_TITLE=str_to_title(CLEANED_JOB_TITLE))
  
  plot_data$specialisms=vacancy_skills$all_skills
  
  plot_data%<>%
    ungroup()%>%
    rowwise()%>%
    mutate(Dimension_1=Dimension_1+runif(1,-1*jitter,jitter),
           Dimension_2=Dimension_2+runif(1,-1*jitter,jitter))%>%
    mutate(specialisms=gsub(',',', ',specialisms),
           Group=paste0('Group ',Cluster,collapse=''))%>%
    rename(`Job Title`=CLEANED_JOB_TITLE,`Skills`=specialisms,`Organisation`=VACANCY_AGENCY_NAME)%>%
    mutate(text=paste(gsub('(.{1,70})(\\s|$)', '\\1\n',paste(
      "<b>", Organisation, "</b>",'<br>')),
      gsub('(.{1,70})(\\s|$)', '\\1\n',paste('Group: ',Group,'<br>')),
      gsub('(.{1,70})(\\s|$)', '\\1\n',paste('Job Title: ',`Job Title`,'<br>')),
      gsub('(.{1,70})(\\s|$)', '\\1\n',paste('Skills: ',Skills,'<br>')),
      gsub('(.{1,70})(\\s|$)', '\\1\n',paste('NHS Pay Band:',`NHS Pay Band`))))
  
  return(plot_data)
}



#' Create an interactive scatter plot using Plotly.
#'
#' This function takes a data.frame created by `create_plot_data` and additional parameters to create an interactive scatter plot using Plotly.
#' It uses the Plotly library to generate a scatter plot with markers representing different groups.
#'
#' @param df A data.frame object.
#' @param x_axis_range Numeric vector specifying the x-axis range (default is c(-0.7, 0.7)).
#' @param y_axis_range Numeric vector specifying the y-axis range (default is c(-0.7, 0.7)).
#'
#' @return Returns the Plotly code for an interactive scatter plot.
#'
#' @export
create_interactive_figure=function(df,x_axis_range=c(-0.7,0.7),y_axis_range=c(-0.7,0.7)){
  
  
  plot_code = df %>%
    plotly::plot_ly(
      .,
      type = 'scatter',
      x =  ~ Dimension_1,
      y =  ~ Dimension_2,
      mode = 'markers',
      color =  ~ Group,
      colors = nhsbsaR::palette_nhsbsa()[1:4],
      text = ~ text,
      hoverinfo = 'text'
    ) %>% config(displayModeBar = FALSE) %>% layout(
      xaxis = list(
        title = '',
        showticklabels = FALSE,
        range = list(x_axis_range[1], x_axis_range[2])
      ),
      yaxis = list(
        showticklabels = FALSE,
        title = "",
        range = list(y_axis_range[1], y_axis_range[2])
      )
    ) %>%
    layout(legend = list(
      orientation = 'h',
      xanchor = 'center',
      x = 0.5,
      y = 1
    ))
  
  
  return(plot_code)
  
}


#' Create tabulated data for DataTable using the DT package.
#'
#' This function takes a data.frame, a vacancy name, and additional parameters to create tabulated data
#' suitable for rendering with the DT package. It generates an interactive DataTable with options for copying,
#' printing, and exporting to Excel or PDF.
#'
#' @param df A data.frame object.
#' @param vacancy_name Name of the vacancy (e.g., 'data_analyst').
#'
#' @return Returns an interactive DataTable.
#'
#' @export

create_tabulated_data=function(df,vacancy_name){
  
  if(vacancy_name=='data_analyst'){
    trgts=c('VAC_REF','Dimension_1','Dimension_2','Type',
            'names','Cluster','text','JOB')
  }
  else{trgts=c('VAC_REF','Dimension_1','Dimension_2','Type',
         'names','Cluster','text')}
  
  DT::datatable(df,
                extensions = 'Buttons',
                rownames = FALSE,
                callback=JS('$("button.buttons-copy").css("background","#005EB8");
                    $("button.buttons-copy").css("color","white");
                    $("button.buttons-print").css("background","#005EB8");
                    $("button.buttons-print").css("color","white");
                    $("button.buttons-excel").css("background","#005EB8");
                    $("button.buttons-excel").css("color","white");
                    $("button.buttons-pdf").css("background","#005EB8");
                    $("button.buttons-pdf").css("color","white");
                    return table;'),
                options = list(dom = 'rtpB',scrollY="300px", pageLength = 10, autoWidth = TRUE,
                               columnDefs = list(list(visible=FALSE,
                                                      targets= trgts )),
                               buttons =list(list(
                                 extend = 'copy',
                                 title = "",
                                 text = 'Copy',
                                 exportOptions = list(
                                   columns = c(1,2,9,7,10)
                                 )) ,list(
                                   extend = 'print',
                                   title = "",
                                   text = 'Print',
                                   exportOptions = list(
                                     columns = c(1,2,9,7,10)
                                   )) ,list(
                                     
                                     extend = 'excel',
                                     filename = paste0(vacancy_name,"_comparison"), 
                                     title = "",
                                     text = 'CSV',
                                     exportOptions = list(
                                       columns = c(1,2,9,7,10)
                                     )
                                   ),list(
                                     extend = 'pdf',
                                     filename = paste0(vacancy_name,"_comparison"), 
                                     title = "",
                                     text = 'PDF',
                                     exportOptions = list(
                                       columns = c(1,2,9,7,10)
                                     )
                                   )
                               )))%>%
    DT::formatStyle(columns = colnames(.$x$data), `font-size` = "14px")
  
  
  
  
}

