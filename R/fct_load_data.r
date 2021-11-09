load_ontario_data <- function() {
    ontario_covid <- readr::read_csv("https://data.ontario.ca/dataset/1115d5fe-dd84-4c69-b5ed-05bf0c0a0ff9/resource/d1bfe1ad-6575-4352-8302-09ca81f7ddfc/download/cases_by_status_and_phu.csv") # nolint

    ontario_covid_daily <- readr::read_csv("https://data.ontario.ca/dataset/f4f86e54-872d-43f8-8a86-3892fd3cb5e6/resource/8a88fe6d-d8fb-41a3-9d04-f0550a44999f/download/daily_change_in_cases_by_phu.csv") # nolint

    on_pop <- readr::read_csv("~/Data Science Work/COVID_DOC/ON_pop.csv")

    on_covid_active <- clean_on_data(ontario_covid, on_pop)

    return(on_covid_active)
}

load_ab_data <- function() {
    alberta_covid <- readr::read_csv("https://www.alberta.ca/data/stats/covid-19-alberta-statistics-data.csv") # nolint

    ab_pop <- readr::read_csv("~/Data Science Work/COVID_DOC/AB_pop.csv")

    ab_covid_active <- clean_ab_data(alberta_covid, ab_pop)

    return(ab_covid_active)
}

load_bc_data <- function() {
    bc_covid <- readr::read_csv("http://www.bccdc.ca/Health-Info-Site/Documents/BCCDC_COVID19_Regional_Summary_Data.csv") # nolint

    bc_pop <- readr::read_csv("~/Data Science Work/COVID_DOC/BC_pop.csv")

    bc_covid_active <- clean_bc_data(bc_covid, bc_pop)
}

load_sk_data <- function() {
    sask_covid <- readr::read_csv("https://dashboard.saskatchewan.ca/export/cases/4017.csv") # nolint

    sk_pop <- readr::read_csv("~/Data Science Work/COVID_DOC/SK_pop.csv")

    sk_covid_active <- clean_sk_data(sask_covid, sk_pop)
}