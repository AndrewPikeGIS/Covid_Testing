load_daily_on_data <- function() {
    ontario_covid_daily <- readr::read_csv("https://data.ontario.ca/dataset/f4f86e54-872d-43f8-8a86-3892fd3cb5e6/resource/8a88fe6d-d8fb-41a3-9d04-f0550a44999f/download/daily_change_in_cases_by_phu.csv") # nolint

    ontario_covid_daily <- clean_on_daily_table(ontario_covid_daily)

    return(ontario_covid_daily)
}

load_on_data <- function() {
    ontario_covid <- readr::read_csv("https://data.ontario.ca/dataset/1115d5fe-dd84-4c69-b5ed-05bf0c0a0ff9/resource/d1bfe1ad-6575-4352-8302-09ca81f7ddfc/download/cases_by_status_and_phu.csv") # nolint

    on_pop <- readr::read_csv("data/ON_pop.csv")

    ontario_covid_data <- clean_on_data(ontario_covid, on_pop)

    return(ontario_covid_data)
}

load_ab_data <- function() {
    alberta_covid <- readr::read_csv("https://www.alberta.ca/data/stats/covid-19-alberta-statistics-data.csv") # nolint

    ab_pop <- readr::read_csv("data/AB_pop.csv")

    alberta_covid_data <- clean_ab_data(alberta_covid, ab_pop)

    return(alberta_covid_data)
}

load_bc_data <- function() {
    bc_covid <- readr::read_csv("http://www.bccdc.ca/Health-Info-Site/Documents/BCCDC_COVID19_Regional_Summary_Data.csv") # nolint

    bc_pop <- readr::read_csv("data/BC_pop.csv")

    bc_covid_data <- clean_bc_data(bc_covid, bc_pop)

    return(bc_covid_data)
}

load_sk_data <- function() {
    #build auto checker to grab the correct csv.
    sask_covid <- automate_sask_download()

    sk_pop <- readr::read_csv("data/SK_pop.csv")

    sk_covid_data <- clean_sk_data(sask_covid, sk_pop)

    return(sk_covid_data)
}


try_load_url <- function(url) {
    response <- tryCatch({
            readr::read_csv(url, guess_max = 15000)
        },
        error = function(cond) {
            return(NA)
        }
    )
    return(response)
}

automate_sask_download <- function() {
    url_return <- NA
    file_num <- 4395
    while (is.na(url_return)) {
        sask_url <- paste0(
            "https://dashboard.saskatchewan.ca/export/cases/",
            file_num,
            ".csv"
        )

        file_num <- file_num + 1
        print(sask_url)
        url_return <- try_load_url(sask_url)

        if (!is.na(url_return)) {
            url_return <- check_for_field(url_return)
        }
    }
    return(url_return)
}

check_for_field <- function(df_in) {
    if ("Region" %in% colnames(df_in)) {
        return(df_in)
    } else {
        return(NA)
    }
}
