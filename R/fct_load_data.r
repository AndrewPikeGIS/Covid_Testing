load_ontario_data <- function() {
    ontario_covid <- read_csv("https://data.ontario.ca/dataset/1115d5fe-dd84-4c69-b5ed-05bf0c0a0ff9/resource/d1bfe1ad-6575-4352-8302-09ca81f7ddfc/download/cases_by_status_and_phu.csv")

    ontario_covid_daily <- read_csv("https://data.ontario.ca/dataset/f4f86e54-872d-43f8-8a86-3892fd3cb5e6/resource/8a88fe6d-d8fb-41a3-9d04-f0550a44999f/download/daily_change_in_cases_by_phu.csv")

    on_pop <- read_csv("~/Data Science Work/COVID_DOC/ON_pop.csv")
}

load_ab_data <- function() {
    alberta_covid <- read_csv("https://www.alberta.ca/data/stats/covid-19-alberta-statistics-data.csv")

    ab_pop <- read_csv("~/Data Science Work/COVID_DOC/AB_pop.csv")
}

load_bc_data <- function() {
    bc_covid <- read_csv("http://www.bccdc.ca/Health-Info-Site/Documents/BCCDC_COVID19_Regional_Summary_Data.csv")

    bc_pop <- read_csv("~/Data Science Work/COVID_DOC/BC_pop.csv")
}

load_sk_data <- function() {
    Sask_COVID <- read_csv("https://dashboard.saskatchewan.ca/export/cases/4017.csv")

    Sk_pop <- read_csv("~/Data Science Work/COVID_DOC/SK_pop.csv")
}