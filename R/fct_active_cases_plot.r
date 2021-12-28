build_active_case_bar_plot <- function(active_case_table, case_field) {
    fig <- plotly::plot_ly(
        x = active_case_table$region,
        y = active_case_table[[case_field]],
        name = stringr::str_to_title(
            gsub("_", " ", case_field)
        ),
        type = "bar",
        hovertext = paste(
            "Region:",
            active_case_table$region,
            "<br>",
            stringr::str_to_title(
                gsub("_", " ", case_field)
            ),
            ":",
            active_case_table[[case_field]]),
        hoverinfo = "text"
    )

    return(fig)
}

add_layout_to_active_case_bar <- function(fig, case_field) {
    fig <- fig %>%
        plotly::layout(
            xaxis = list(
                title = "Region",
                tickangle = -45),
            yaxis = list(
                title = stringr::str_to_title(
                    gsub("_", " ", case_field)
                    )
                ),
            margin = list(b = 150),
            title = paste(
                stringr::str_to_title(
                    gsub("_", " ", case_field)
                ),
                "by Region"
            )
        )
    return(fig)
}
