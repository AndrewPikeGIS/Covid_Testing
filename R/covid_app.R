#' Title
#'
#' @return
#' @export
#' @import shiny
#' @examples

covid_app <- function() {
    ab_covid_active <- load_ab_data()
    bc_covid_active <- load_bc_data()
    on_covid_active <- load_on_data()
    sk_covid_active <- load_sk_data()

    merged_covid_data <- clean_merge_active_cases_data(
        ab_covid_active,
        bc_covid_active,
        on_covid_active,
        sk_covid_active)


    ui <- bs4Dash::dashboardPage(
        fullscreen = TRUE,
        bs4Dash::dashboardHeader(title = "Covid 19 Case Counts"),
        bs4Dash::dashboardSidebar(
            bs4Dash::sidebarMenu(
                bs4Dash::menuItem(
                    "Covid Case Count",
                    tabName = "covid_case_count",
                    icon = icon("chart-area")
                ),
                bs4Dash::menuItem(
                    "AB, BC, ON, SK Cases Table",
                    tabName = "case_table",
                    icon = icon("th")
                )
            )
        ),
        bs4Dash::dashboardBody(
            bs4Dash::tabItems(
                bs4Dash::tabItem(
                    tabName = "covid_case_count",
                    fluidRow(
                        bs4Dash::box(
                            title = "Covid-19 Case Count",
                            width = 12
                        )
                    )
                ),
                bs4Dash::tabItem(
                    tabName = "case_table",
                    fillPage(
                        mod_cad_case_table_ui("case_table")
                    )
                )
            )
        )
    )

    server <- function(input, output, session) {
        mod_cad_case_table_server(
            "case_table",
            merged_covid_data
        )
    }


    shinyApp(ui, server)
}