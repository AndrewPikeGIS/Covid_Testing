clean_ontario_data <- function(ontario_covid, on_pop) {
    #create join fields for overal merge dataset
    ontario_covid <- ontario_covid %>%
        mutate(
            prov = "ON",
            region = PHU_NAME) %>%
        drop_na(region)

    #fix the region names for the ontario population table
    on_pop <- on_pop %>%
        mutate(
            region = toupper(region),
            density = Population / area)

    #Join population data to the covid dataset
    ontario_covid <- left_join(
        ontario_covid,
        on_pop,
        by = "region")

    #filter the ontario data to active cases
    ontario_covid_active <- ontario_covid %>%
        filter(FILE_DATE == max(FILE_DATE)) %>%
        select(
            "prov",
            "region",
            "ACTIVE_CASES",
            "Population",
            "density")

}

clean_ab_data <- function(alberta_covid, ab_pop) {
    #Fix alberta covid dataset for the canada wide merge
    alberta_covid <- alberta_covid %>%
        mutate(
            prov = "AB",
            region = toupper(`Alberta Health Services Zone`),
            age = ab_age(`Age group`)) %>%
        drop_na(region)

    ab_pop <- ab_pop %>%
        mutate(density = Population / area)

    #merge the Alberta population dat to the covid dataset.
    alberta_covid <- left_join(
        alberta_covid,
        ab_pop,
        by = "region")

    #filter the AB data to active cases
    alberta_covid_active <- alberta_covid %>%
        filter(`Case status` == "Active") %>%
        group_by(region, Population, density) %>%
        summarize(ACTIVE_CASES = n()) %>%
        mutate(prov = "AB")

}

clean_bc_data <- function(bc_covid, bc_pop) {

    #fix the bc covid dataset
    bc_covid <- bc_covid %>%
        mutate(
            prov = "BC",
            region = HA,
            ACTIVE_CASES = Cases_Reported)

    bc_pop <- bc_pop %>%
    mutate(density = Population / area)

    #join the populationd data to the bc covid data
    bc_covid <- left_join(
        bc_covid,
        bc_pop)

    #filter the bc dat to the best approximation of active cases
    bc_covid_active <- bc_covid %>%
        filter(
            Date >= (max(Date) - days(14)) &
            region != "All" &
            HSDA != "All") %>%
        group_by(region, Population, density) %>%
        summarize(ACTIVE_CASES = sum(Cases_Reported)) %>%
        mutate(prov = "BC")
}