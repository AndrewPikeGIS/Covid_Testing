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

clean_on_daily_table <- function(on_daily_covid_data) {
    #colnames(on_daily_covid_data) <- ontario_daily_col_names()
    on_daily_covid_cols <- colnames(on_daily_covid_data)
    piv_cols <- on_daily_covid_cols[-1]

    on_daily_covid_data_long <- on_daily_covid_data %>%
        tidyr::pivot_longer(
            cols = all_of(piv_cols),
            names_to = "region",
            values_to = "daily_cases")

    startdate <- min(on_daily_covid_data_long$Date)

    on_daily_covid_data_long <- on_daily_covid_data_long %>%
        dplyr::mutate(
            day_from_start = as.numeric(day_count(startdate, Date)),
            prov = "ON",
            region = gsub("_", " ", region),
            region = stringr::str_to_title(region)
        ) %>%
        dplyr::rename(
            date = Date
        )
    return(on_daily_covid_data_long)
}

ab_age <- function(age_in) {
  age_split <- strsplit(age_in, "-")[[1]][1]
  age <- ifelse(age_split == "Under 1 year", 1, strtoi(age_split))
  return(age)
}

clean_ab_data <- function(alberta_covid, ab_pop) {

    #Fix alberta covid dataset for the canada wide merge
    alberta_covid <- alberta_covid %>%
        dplyr::mutate(
            prov = "AB",
            region = toupper(`Alberta Health Services Zone`),
            age = ab_age(`Age group`)) %>%
        tidyr::drop_na(region)

    ab_pop <- ab_pop %>%
        dplyr::mutate(density = Population / area)

    ab_pop <- ab_pop %>%
        janitor::adorn_totals("row")
    #merge the Alberta population dat to the covid dataset.
    alberta_covid <- dplyr::left_join(
        alberta_covid,
        ab_pop,
        by = "region")

    #need to aggregate to daily new cases etc.

    return(alberta_covid)
}


day_count <- function(date_start, date_end) {
        x <- lubridate::interval(date_start, date_end)
        return(abs(x %/% lubridate::days(1)))
    }

create_ab_daily_cases_table <- function(alberta_covid_data) {

    ab_daily_cases <- alberta_covid_data %>%
        dplyr::group_by(`Date reported`, region) %>%
        dplyr::summarize(cases_reported = dplyr::n()) %>%
        dplyr::rename(date = `Date reported`)

    startdate <- min(ab_daily_cases$date)

    ab_daily_cases <- ab_daily_cases %>%
        dplyr::mutate(
            day_from_start = as.numeric(day_count(startdate, date)),
            prov = "AB"
        ) %>%
        dplyr::ungroup() %>%
        dplyr::rename(
            daily_cases = cases_reported
        )
    return(ab_daily_cases)
}

clean_bc_data <- function(bc_covid, bc_pop) {

    #fix the bc covid dataset
    bc_covid <- bc_covid %>%
        dplyr::mutate(
            prov = "BC",
            region = HA) %>%
        dplyr::rename(
            date = Date,
            new_cases = Cases_Reported,
            hsda = HSDA)

    bc_pop <- bc_pop %>%
        dplyr::mutate(density = Population / area)

    #join the populationd data to the bc covid data
    bc_covid <- dplyr::left_join(
        bc_covid,
        bc_pop)

    return(bc_covid)
}

clean_bc_for_daily_plt <- function(bc_covid_data) {
    #make the filter dynamic on selection in reactable.
    bc_daily <- bc_covid_data %>%
        dplyr::filter(
            region != "Unknown",
            region != "Out of Canada"
        ) %>%
        dplyr::group_by(region, date) %>%
        dplyr::summarise(
            daily_cases = sum(new_cases),
            daily_cases_smooth = sum(Cases_Reported_Smoothed)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            prov = "BC"
        )

    return(bc_daily)
}

clean_sk_data <- function(sask_covid, sk_pop) {
    #clean up sask data
    startdate <- min(sask_covid$Date)

    sask_covid <- sask_covid %>%
        dplyr::group_by(
            Date,
            Region) %>%
        dplyr::summarize(
            new_cases = sum(`New Cases`),
            active_cases = sum(`Active Cases`),
            resolved_cases = sum(`Recovered Cases`),
            inpatient_hospitalizations = sum(`Inpatient Hospitalizations`),
            icu_hospitalizations = sum(`ICU Hospitalizations`),
            total_cases =  sum(`Total Cases`)) %>%
        dplyr::ungroup() %>%
        dplyr::rename(
            date = Date,
            region = Region,
            daily_cases = new_cases
        ) %>%
        dplyr::mutate(
            day_from_start = as.numeric(day_count(startdate, date)),
            prov = "SK"
        )

    sk_pop <- sk_pop %>%
        dplyr::mutate(
            density = Population / area,
            prov = "SK")

    sask_covid_data <- dplyr::left_join(
        sask_covid,
        sk_pop)
    return(sask_covid_data)
}

create_sk_active_table <- function(sk_covid_data) {
    sk_active_covid <- sk_covid_data %>%
        filter(Date == max(Date))

    return(sk_active_covid)
}

clean_sk_table_for_daily <- function(sk_covid_data) {
    sk_covid_daily <- sk_covid_data %>%
        dplyr::select(
            region,
            prov,
            date,
            daily_cases,
            day_from_start
        ) %>%
        dplyr::filter(
            region != "Total"
        )
    return(sk_covid_daily)
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
            cases_per_100k = active_cases / (Population / 100000),
            cases_times_density = active_cases * density,
            region = stringr::str_to_title(region),
            region = dplyr::case_when(
                region == "Oxford Elgin-St.thomas" ~ "Oxford Elgin-St. Thomas",
                TRUE ~ region
            )) %>%
        dplyr::filter(active_cases > 0) %>%
        dplyr::select(
            region,
            prov,
            Population,
            active_cases,
            cases_per_100k
        ) %>%
        dplyr::ungroup()

    return(merge_covid_active)
}

run_loess <- function(daily_df) {
    fit <- daily_df %>%
        tidyr::drop_na() %>%
        tidyr::nest(-region) %>%
        dplyr::mutate(
            m = purrr::map(
                data,
                loess,
                formula = daily_cases ~ day_from_start,
                span = 0.04
            ),
            fitted = purrr::map(m, `[[`, "fitted")
        )

    pred <- fit %>%
        dplyr::select(-m) %>%
        tidyr::unnest(
            cols = c(
                data,
                fitted
            )
        )

    pred <- pred %>%
        dplyr::rename(
            daily_cases_smooth = fitted
        ) %>%
        dplyr::mutate(
            daily_cases_smooth = dplyr::if_else(
                daily_cases_smooth < 0, 0, daily_cases_smooth
            )
        )

    return(pred)
}

add_color_to_region_name <- function(daily_case_df) {
    palette_region <- grDevices::colorRampPalette(c(
        "darkblue",
        "blue",
        "darkgreen",
        "green",
        "lightgreen",
        "yellow",
        "orange",
        "red",
        "darkred",
        "violet",
        "purple",
        "#2f014b",
        "#13011f",
        "#000000"
        ),
        bias = 1
    )

    palette_region(length(unique(daily_case_df$region_name)))

    daily_case_df$region_name <- factor(
        daily_case_df$region_name,
        labels = palette_region(length(unique(daily_case_df$region_name)))
    )

    return(daily_case_df)
}

merge_daily_covid_tables <- function(
    ab_table,
    bc_table,
    sk_table,
    on_table
) {
    ab_table <- ab_table %>%
        dplyr::select(
            region,
            prov,
            date,
            daily_cases,
            daily_cases_smooth
        )

    bc_table <- bc_table %>%
        dplyr::select(
            region,
            prov,
            date,
            daily_cases,
            daily_cases_smooth
        )

    sk_table <- sk_table %>%
        dplyr::select(
            region,
            prov,
            date,
            daily_cases,
            daily_cases_smooth
        )

    on_table <- on_table %>%
        dplyr::select(
            region,
            prov,
            date,
            daily_cases,
            daily_cases_smooth
        )

    merged_daily_table <- rbind(
        ab_table,
        bc_table,
        on_table,
        sk_table
    ) %>%
    dplyr::mutate(
        region = stringr::str_to_title(region),
        region = dplyr::case_when(
            region == "Chatham Kent" ~ "Chatham-Kent",
            region == "Haldimand Norfolk" ~ "Haldimand-Norfolk",
            region == "Haliburton Kawartha Pine Ridge" ~ "Haliburton, Kawartha, Pine Ridge",    # nolint
            region == "Hastings Prince Edward" ~ "Hastings & Prince Edward Counties",           # nolint
            region == "Kfla" ~ "Kingston, Frontenac, Lennox & Addington",
            region == "Leeds Grenville Lanark" ~ "Leeds, Grenville And Lanark District",        # nolint
            region == "Middlesex London" ~ "Middlesex-London",
            region == "Southwestern" ~ "Oxford Elgin-St. Thomas",
            region == "Peterborough County City" ~ "Peterborough County-City",
            region == "Wellington Dufferin Guelph" ~ "Wellington-Dufferin-Guelph",              #nolint
            region == "Windsor Essex County" ~ "Windsor-Essex County",
            TRUE ~ region
        ),
        region_name = paste(region, prov),
    )

    return(merged_daily_table)
}

add_prov_totals_active <- function(df_in) {

    agg_table <- df_in %>%
        dplyr::group_by(prov) %>%
        dplyr::summarise(
            active_cases = sum(active_cases),
            Population = sum(Population)
        ) %>%
        dplyr::mutate(
            region = paste("Total", prov),
            cases_per_100k = active_cases / (Population / 100000)
        )

    df_out <- dplyr::bind_rows(df_in, agg_table) %>%
        dplyr::arrange(prov, region)
    return(df_out)
}

add_prov_totals_daily <- function(df_in) {
    startdate <- min(df_in$date)

    agg_table <- df_in %>%
        dplyr::group_by(prov, date) %>%
        dplyr::summarise(
            daily_cases = sum(daily_cases),
        ) %>%
        dplyr::mutate(
            region = paste("Total", prov),
            region_name = region,
            day_from_start = as.numeric(day_count(startdate, date))
        ) %>%
        dplyr::arrange(
            region,
            date
        )

    df_smooth <- run_loess(agg_table) %>%
        dplyr::select(
            -day_from_start
        )

    df_out <- dplyr::bind_rows(df_in, df_smooth) %>%
        dplyr::arrange(prov, region)
    return(df_out)
}