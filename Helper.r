ab_age <- function(strAge) {
  age_split <- strsplit(strAge, '-')[[1]][1]
  age <- ifelse(age_split =="Under 1 year", 1, strtoi(age_split))
}

add_prov <- function(prov, region){
  return(paste(region, prov))
}