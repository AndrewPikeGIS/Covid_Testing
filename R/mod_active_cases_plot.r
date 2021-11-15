mod_active_cases_plot_ui <- function(id) {
    ns <- NS(id)
    plotly::plotlyOutput(ns("active_case_plot"), height = "90vh")
}

mod_active_cases_plot_server <- function(id,
                                         active_case_table,
                                         region_selection) {

    moduleServer(id, function(input, output, session) {
        output$active_case_plot <- plotly::renderPlotly({
            active_case_table %>%
                filter_for_active_reactable() %>%
                build_active_case_reactable()
        })
    })
}