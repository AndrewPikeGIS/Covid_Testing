#' Title
#'
#' @return
#' @export
#' @import shiny
#' @examples

covid_app <- function() {
    ontario_covid_data <- load_on_data()

    ontario_active_cases <- create_active_on_table(ontario_covid_data)

    ontario_covid_daily_data <- load_daily_on_data()

    alberta_covid_data <- load_ab_data()

    alberta_active_cases <- create_active_ab_table(alberta_covid_data)

    alberta_daily_covid_data <- create_ab_daily_cases_table(alberta_covid_data)

    bc_covid_data <- load_bc_data()

    bc_active_cases <- create_active_bc_table(bc_covid_data)

    sk_covid_data <- load_sk_data()

    sk_active_cases <- create_active_sk_table(sk_covid_data)

    merged_covid_data <- clean_merge_active_cases_data(
        alberta_active_cases,
        ontario_active_cases,
        bc_active_cases,
        sk_active_cases)

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