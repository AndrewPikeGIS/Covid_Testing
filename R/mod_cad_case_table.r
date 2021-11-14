mod_cad_case_table_ui <- function(id) {
    ns <- NS(id)
    reactable::reactableOutput(ns("table"), height = "90vh")
}

mod_cad_case_table_server <- function(id, active_case_table) {
    moduleServer(id, function(input, output, session) {
        output$table <- reactable::renderReactable(
            active_case_table %>%
                filter_for_active_reactable() %>%
                build_active_case_reactable()
        )

        current_selection <- reactive(
            reactable::getReactableState("table", "selected")
        )

        selected_regions <- reactive(
            active_case_table$region[current_selection()]
        )

        return(selected_regions)
    })
}