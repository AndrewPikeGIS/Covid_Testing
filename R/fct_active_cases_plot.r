filter_to_selected_regions <- function(active_case_table, selected_regions) {
    table_out <- active_case_table %>%
        dplyr::filter(region %in% selected_regions)
}