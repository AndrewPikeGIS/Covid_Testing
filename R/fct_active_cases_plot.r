build_empty_plot <- function(plot_name) {
    fig <- plotly::plot_ly(name = plot_name)
    return(fig)
}

build_active_case_bar_plot <- function(active_case_table) {
    active_case_table %>%
        plotly::plot_ly(
            x = ~region,
            y = ~active_cases,
            type = "bar"
    )
}