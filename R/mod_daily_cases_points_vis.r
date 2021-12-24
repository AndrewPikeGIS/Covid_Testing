mod_daily_cases_point_ui <- function(id) {
    ns <- NS(id)
    checkboxInput(
        ns("daily_points_vis"),
        "Add Daily Case Points To Plot",
        FALSE
    )
}

mod_daily_cases_point_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        daily_points_vis <- reactive(
            input$daily_points_vis
        )

        return(daily_points_vis)
    })
}