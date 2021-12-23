build_active_case_bar_plot <- function(active_case_table, case_field) {
    plotly::plot_ly(
            x = active_case_table$region,
            y = active_case_table[[case_field]],
            name = "Active Covid Cases",
            type = "bar"
    )
}