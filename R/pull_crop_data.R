pull_crop_data <- function(){
  params <- yaml::read_yaml(here::here("params.yml"))
  dta <- data.table::fread(params$source$url)[ STATE_ALPHA %in% params$source$state_abbr & COUNTY_NAME != 'NULL']

  dta$DOMAINCAT_DESC <- ifelse(dta$DOMAINCAT_DESC == '', NA, dta$DOMAINCAT_DESC)

  readr::write_csv(dta, here::here("data/2017_agcensus_ia_co.csv"))
}
