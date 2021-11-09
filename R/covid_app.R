covid_app <- function() {
    ab_covid_active <- load_ab_data()
    bc_covid_active <- load_bc_data()
    on_covid_active <- load_on_data()
    sk_covid_active <- load_sk_data()

    merged_covid_data <- clean_merge_covid_data(
        ab_covid_active,
        bc_covid_active,
        on_covid_active,
        sk_covid_active)
}