build_daily_case_plot <- function(daily_cases_df) {
    fig <- daily_cases_df %>%
        dplyr::group_by(region_name) %>%
        plotly::plot_ly() %>%
        plotly::add_trace(
            x = ~date,
            y = ~daily_cases_smooth,
            type = "scatter",
            mode = "lines",
            name = ~region,
            line = list(color = ~region_name),
            hovertext = ~paste(
                "Region:",
                region,
                prov,
                "<br>Smoothed Daily Cases:",
                daily_cases_smooth,
                "<br>Date:",
                date),
            hoverinfo = "text"
        )
    return(fig)
}

add_layout_to_daily_case_plot <- function(fig) {
    fig <- fig %>% plotly::layout(
        xaxis = list(
            title = "Date Reported",
            tickangle = -45),
        yaxis = list(title = "Daily Cases"),
        margin = list(b = 150),
        title = "Daily Covid Cases Reported") %>%
        plotly::rangeslider() %>%
        plotly::toWebGL()
    return(fig)
}

add_points_trace_to_daily_plot <- function(fig,
                                           daily_cases_df,
                                           points_visible) {
    fig <- fig %>%
        plotly::add_trace(
            x = daily_cases_df$date,
            y = daily_cases_df$daily_cases,
            #type = "scatter",
            mode = "markers",
            marker = list(color = daily_cases_df$region_name),
            name = daily_cases_df$region,
            opacity = 0.5,
            hovertext = paste(
                "Region:",
                daily_cases_df$region,
                daily_cases_df$prov,
                "<br>Smoothed Daily Cases:",
                daily_cases_df$daily_cases,
                "<br>Date:",
                daily_cases_df$date),
            hoverinfo = "text",
            visible = points_visible
        )
    return(fig)
}