mod_active_cases_plot_ui <- function(id) {
    ns <- NS(id)
    plotly::plotlyOutput(ns("active_case_plot"), height = "90vh")
}

mod_active_cases_plot_server <- function(id,
                                         active_case_table,
                                         current_selection) {

    moduleServer(id, function(input, output, session) {

        selected_table <- reactive(
            active_case_table[current_selection(), ]
        )

        output$active_case_plot <- plotly::renderPlotly({
            build_active_case_bar_plot(selected_table())
        })
    })
}
