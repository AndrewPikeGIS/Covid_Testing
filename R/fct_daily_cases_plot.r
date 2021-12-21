build_daily_case_plot <- function(daily_cases_df) {
    daily_cases_df %>%
        dplyr::group_by(region_name) %>%
        plotly::plot_ly(
            x = ~date,
            y = ~daily_cases_smooth,
            type = "scatter",
            mode = "line",
            name = ~region_name,
            hovertext = ~paste(
                "Region:",
                region,
                "<br>Smoothed Daily Cases:",
                daily_cases_smooth,
                "<br>Date:",
                date),
            hoverinfo = "text") %>%
        plotly::layout(
            xaxis = list(
                title = "Date Reported",
                tickangle = -45),
            yaxis = list(title = "Daily Cases"),
            margin = list(b = 150),
            title = "Daily Covid Cases Reported") %>%
        plotly::rangeslider()
}
