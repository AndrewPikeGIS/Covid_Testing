clean_on_data <- function(ontario_covid, on_pop) {
    #create join fields for overal merge dataset
    ontario_covid <- ontario_covid %>%
        dplyr::mutate(
            prov = "ON",
            region = PHU_NAME) %>%
        dplyr::drop_na(region)

    #fix the region names for the ontario population table
    on_pop <- on_pop %>%
        dplyr::mutate(
            region = toupper(region),
            density = Population / area)

    #Join population data to the covid dataset
    ontario_covid <- dplyr::left_join(
        ontario_covid,
        on_pop,
        by = "region")

    #filter the ontario data to active cases
    ontario_covid_active <- ontario_covid %>%
        dplyr::filter(FILE_DATE == max(FILE_DATE)) %>%
        dplyr::select(
            "prov",
            "region",
            "ACTIVE_CASES",
            "Population",
            "density")

    return(ontario_covid_active)
}

clean_ab_data <- function(alberta_covid, ab_pop) {
    #Fix alberta covid dataset for the canada wide merge
    alberta_covid <- alberta_covid %>%
        dplyr::mutate(
            prov = "AB",
            region = toupper(`Alberta Health Services Zone`),
            age = ab_age(`Age group`)) %>%
        dplyr::drop_na(region)

    ab_pop <- ab_pop %>%
        dplyr::mutate(density = Population / area)

    #merge the Alberta population dat to the covid dataset.
    alberta_covid <- dplyr::left_join(
        alberta_covid,
        ab_pop,
        by = "region")

    #filter the AB data to active cases
    alberta_covid_active <- alberta_covid %>%
        dplyr::filter(`Case status` == "Active") %>%
        dplyr::group_by(region, Population, density) %>%
        dplyr::summarize(ACTIVE_CASES = n()) %>%
        dplyr::mutate(prov = "AB")

    return(alberta_covid_active)
}

clean_bc_data <- function(bc_covid, bc_pop) {

    #fix the bc covid dataset
    bc_covid <- bc_covid %>%
        dplyr::mutate(
            prov = "BC",
            region = HA,
            ACTIVE_CASES = Cases_Reported)

    bc_pop <- bc_pop %>%
        dplyr::mutate(density = Population / area)

    #join the populationd data to the bc covid data
    bc_covid <- dplyr::left_join(
        bc_covid,
        bc_pop)

    #filter the bc dat to the best approximation of active cases
    bc_covid_active <- bc_covid %>%
        dplyr::filter(
            Date >= (max(Date) - days(14)) &
            region != "All" &
            HSDA != "All") %>%
        dplyr::group_by(region, Population, density) %>%
        dplyr::summarize(ACTIVE_CASES = sum(Cases_Reported)) %>%
        dplyr::mutate(prov = "BC")

    return(bc_covid_active)
}

clean_sk_data <- function(sask_covid, sk_pop) {
    #clean up sask data
    sask_covid <- sask_covid %>%
        dplyr::group_by(
            Date,
            Region) %>%
        dplyr::summarize(
            New_Cases = sum(`New Cases`),
            Active_Cases = sum(`Active Cases`)) %>%
        dplyr::ungroup()

    colnames(sask_covid) <- c(
        "Date",
        "region",
        "New_Cases",
        "ACTIVE_CASES")

    sask_covid_active <- sask_covid %>%
        dplyr::filter(Date == max(Date))

    sk_pop <- sk_pop %>%
        dplyr::mutate(
            density = Population / area,
            prov = "SK")

    sask_covid_active <- dplyr::left_join(
        sask_covid_active,
        sk_pop)

    sask_covid_active <- sask_covid_active %>%
        dplyr::select(
            "prov",
            "region",
            "ACTIVE_CASES",
            "Population",
            "density")

    return(sask_covid_active)
}

clean_merge_covid_data <- function(alberta_covid_active,
                                   bc_covid_active,
                                   ontario_covid_active,
                                   sask_covid_active) {

    #merge to the final canada wide dataset
    merge_covid_active <- dplyr::bind_rows(
        alberta_covid_active,
        bc_covid_active,
        ontario_covid_active,
        sask_covid_active)

    merge_covid_active <- merge_covid_active %>%
        dplyr::drop_na(Population) %>%
        dplyr::mutate(
            region = as.factor(add_prov(prov, region)),
            cases_per_100k = ACTIVE_CASES / (Population / 100000),
            cases_times_density = ACTIVE_CASES * density) %>%
        dplyr::filter(ACTIVE_CASES > 0) %>%
        arrange(desc(ACTIVE_CASES))

    merge_covid_active$region <- factor(
        merge_covid_active$region,
        levels = unique(merge_covid_active$region)
        [order(merge_covid_active$ACTIVE_CASES, decreasing = TRUE)])

    return(merge_covid_active)
}