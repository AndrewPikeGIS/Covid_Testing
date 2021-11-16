build_empty_plot <- function(plot_name) {
    fig <- plotly::plot_ly(name = plot_name)
    return(fig)
}

add_active_case_bar_trace <- function(fig, active_case_table) {
    fig <- fig %>%
        plotly::add_trace(
            type = "bar",
            x = active_case_table$region,
            y = active_case_table$active_cases
        )

    return(fig)
}