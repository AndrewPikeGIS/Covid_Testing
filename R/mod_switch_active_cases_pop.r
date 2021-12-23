mod_switch_act_plot_pop_ui <- function(id) {
    ns <- NS(id)
    checkboxInput(
        ns("pop_per_cap"),
        "Show Normalized Case Count\n(per 100k pop)",
        FALSE
    )
}

mod_switch_act_plot_pop_server <- function(id) {
    moduleServer(id, function(input, output, session) {

        case_field <- reactive(
            dplyr::if_else(
                isTRUE(input$pop_per_cap),
                "cases_per_100k",
                "active_cases"
            )
        )
        return(case_field)
    })
}