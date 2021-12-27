mod_daily_cases_plot_ui <- function(id) {
    ns <- NS(id)
    plotly::plotlyOutput(ns("daily_case_plot"), height = "82vh")
}

mod_daily_cases_plot_server <- function(id,
                                        daily_case_table,
                                        current_selection,
                                        active_case_table,
                                        points_visible) {
    moduleServer(id, function(input, output, session) {

        selected_table <- reactive(
            subset(
                daily_case_table,
                region %in%
                as.vector(active_case_table$region[current_selection()]))
        )

        output$daily_case_plot <- plotly::renderPlotly({
            build_daily_case_plot(selected_table()) %>%
            add_points_trace_to_daily_plot(
                selected_table(),
                points_visible()
            ) %>%
            add_layout_to_daily_case_plot()
        })
    })
}
