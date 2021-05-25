transform_types <- function(df) {
  df$city <- as.factor(df$city)
  df$gender <- as.factor(df$gender)
  df$relevent_experience <- as.factor(df$relevent_experience)
  df$enrolled_university <- as.factor(df$enrolled_university)
  df$education_level <- as.factor(df$education_level)
  df$major_discipline <- as.factor(df$major_discipline)
  df$experience <- as.factor(df$experience)
  df$company_size <- as.factor(df$company_size)
  df$company_type <- as.factor(df$company_type)
  df$last_new_job <- as.factor(df$last_new_job)
  df$training_hours <- as.integer(df$training_hours)

  return(df)
}