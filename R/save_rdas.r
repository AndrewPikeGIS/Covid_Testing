save_all_rdas <- function(
    ontario_covid_data,
    ontario_covid_daily_data,
    alberta_covid_data,
    bc_covid_data,
    sk_covid_data
) {
    save(
        ontario_covid_data,
        file = "data/on_covid.rda"

    )

    save(
        ontario_covid_daily_data,
        file = "data/on_daily.rda"
    )

    save(
        alberta_covid_data,
        file = "data/ab_covid.rda"
    )

    save(
        bc_covid_data,
        file = "data/bc_covid.rda"
    )

    save(
        sk_covid_data,
        file = "data/sk_covid.rda"
    )
}

#save_all_rdas(ontario_covid_data, ontario_covid_daily_data, alberta_covid_data, bc_covid_data, sk_covid_data)       # nolint
