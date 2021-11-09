clean_ontario_data <- function(ontario_covid) {
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