clean_on_data <- function(ontario_covid, on_pop) {
    #create join fields for overal merge dataset
    ontario_covid <- ontario_covid %>%
        dplyr::mutate(
            prov = "ON",
            region = PHU_NAME) %>%
        tidyr::drop_na(region) %>%
        dplyr::group_by(region) %>%
        dplyr::mutate(
            new_cases = ACTIVE_CASES - dplyr::lag(
                ACTIVE_CASES, 1, default = NA
                ) + RESOLVED_CASES,
            new_cases = ifelse(
                is.na(new_cases),
                ACTIVE_CASES + RESOLVED_CASES,
                new_cases),
            new_cases = ifelse(
                new_cases < 0,
                0,
                new_cases
            )
        ) %>%
        dplyr::rename(
            active_cases = ACTIVE_CASES,
            resolved_cases = RESOLVED_CASES,
            date = FILE_DATE) %>%
        dplyr::ungroup()


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

    return(ontario_covid)
}

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

ab_age <- function(age_in) {
  age_split <- strsplit(age_in, "-")[[1]][1]
  age <- ifelse(age_split == "Under 1 year", 1, strtoi(age_split))
  return(age)
}

add_prov <- function(prov, region) {
  return(paste(region, prov))
}

create_active_ab_table <- function(alberta_covid, ab_pop) {
    #Fix alberta covid dataset for the canada wide merge
    alberta_covid <- alberta_covid %>%
        dplyr::mutate(
            prov = "AB",
            region = toupper(`Alberta Health Services Zone`),
            age = ab_age(`Age group`)) %>%
        tidyr::drop_na(region)

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
        dplyr::summarize(ACTIVE_CASES = dplyr::n()) %>%
        dplyr::mutate(prov = "AB")
    return(alberta_covid_active)
}

create_active_bc_table <- function(bc_covid, bc_pop) {

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
            Date >= (max(Date) - lubridate::days(14)) &
            region != "All" &
            HSDA != "All") %>%
        dplyr::group_by(region, Population, density) %>%
        dplyr::summarize(ACTIVE_CASES = sum(Cases_Reported)) %>%
        dplyr::mutate(prov = "BC")

    return(bc_covid_active)
}

create_active_sk_table <- function(sask_covid, sk_pop) {
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

clean_merge_active_cases_data <- function(alberta_covid_active,
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
        tidyr::drop_na(Population) %>%
        dplyr::mutate(
            region = as.factor(add_prov(prov, region)),
            cases_per_100k = ACTIVE_CASES / (Population / 100000),
            cases_times_density = ACTIVE_CASES * density) %>%
        dplyr::filter(ACTIVE_CASES > 0) %>%
        dplyr::arrange(desc(ACTIVE_CASES))

    merge_covid_active$region <- factor(
        merge_covid_active$region,
        levels = unique(merge_covid_active$region)
        [order(merge_covid_active$ACTIVE_CASES, decreasing = TRUE)])

    return(merge_covid_active)
}