mod_cad_case_table_ui <- function(id) {
    ns <- NS(id)
    reactable::reactableOutput(ns("table"), height = "90vh")
}

mod_cad_case_table_server <- function(id, cad_case_table) {
    moduleServer(id, function(input, output, session) {
        output$table <- reactable::renderReactable(
            cad_case_table %>%
                build_cad_case_reactable()
        )

        current_selection <- reactive(
            reactable::getReactableState("table", "selected")
        )
    })
}