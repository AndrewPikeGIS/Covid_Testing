create_active_on_table <- function(ontario_covid) {

    #filter the ontario data to active cases
    ontario_covid_active <- ontario_covid %>%
        dplyr::filter(date == max(date)) %>%
        dplyr::select(
            "prov",
            "region",
            "active_cases",
            "new_cases",
            "resolved_cases",
            "Population",
            "density")

    return(ontario_covid_active)
}

create_active_ab_table <- function(alberta_covid) {
    #filter the AB data to active cases
    alberta_covid_active <- alberta_covid %>%
        dplyr::filter(`Case status` == "Active") %>%
        dplyr::group_by(region, Population, density) %>%
        dplyr::summarize(active_cases = dplyr::n()) %>%
        dplyr::mutate(prov = "AB")
    return(alberta_covid_active)
}

create_active_bc_table <- function(bc_covid) {
    #filter the bc dat to the best approximation of active cases
    bc_covid_active <- bc_covid %>%
        dplyr::filter(
            date >= (max(date) - lubridate::days(14)) &
            region != "All" &
            hsda != "All") %>%
        dplyr::group_by(region, Population, density) %>%
        dplyr::summarize(active_cases = sum(new_cases)) %>%
        dplyr::mutate(prov = "BC")

    return(bc_covid_active)
}

create_active_sk_table <- function(sask_covid_data) {
    sask_covid_active <- sask_covid_data %>%
        dplyr::filter(date == max(date)) %>%

    return(sask_covid_active)
}
