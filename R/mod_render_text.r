mod_render_text_ui <- function(id) {
    ns <- NS(id)
    textOutput(ns("disclaimer_text"))
}

mod_render_text_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        output$disclaimer_text <- renderText({
            build_disclaimer()
        })
    })
}