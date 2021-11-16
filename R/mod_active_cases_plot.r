mod_active_cases_plot_ui <- function(id) {
    ns <- NS(id)
    plotly::plotlyOutput(ns("active_case_plot"), height = "90vh")
}

mod_active_cases_plot_server <- function(id, active_case_table) {
    moduleServer(id, function(input, output, session) {

        output$active_case_plot <- plotly::renderPlotly({
            build_empty_plot() %>%
                add_active_case_bar_trace(active_case_table)
        })
    })
}