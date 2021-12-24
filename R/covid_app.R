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

    ontario_covid_daily_data <- run_loess(ontario_covid_daily_data)

    alberta_covid_data <- load_ab_data()

    alberta_active_cases <- create_active_ab_table(alberta_covid_data)

    alberta_daily_covid_data <- create_ab_daily_cases_table(alberta_covid_data)

    alberta_daily_covid_data <- run_loess(alberta_daily_covid_data)

    bc_covid_data <- load_bc_data()

    bc_active_cases <- create_active_bc_table(bc_covid_data)

    bc_covid_daily <- clean_bc_for_daily_plt(bc_covid_data)

    sk_covid_data <- load_sk_data()

    sk_actve_cases <- create_active_sk_table(sk_covid_data)

    sk_covid_daily_data <- clean_sk_table_for_daily(sk_covid_data)

    sk_covid_daily_data <- run_loess(sk_covid_daily_data)

    merged_active_cases <- clean_merge_active_cases_data(
        alberta_active_cases,
        ontario_active_cases,
        bc_active_cases,
        sk_actve_cases)

    merged_daily_table <- merge_daily_covid_tables(
        alberta_daily_covid_data,
        bc_covid_daily,
        ontario_covid_daily_data,
        sk_covid_daily_data
    ) %>%
    add_color_to_region_name()

    #load("data\\merged_cases.rda")

    ui <- bs4Dash::dashboardPage(
        fullscreen = TRUE,
        bs4Dash::dashboardHeader(title = "Covid 19 Case Counts"),
        bs4Dash::dashboardSidebar(
            bs4Dash::sidebarMenu(
                bs4Dash::menuItem(
                    "Active Covid Cases",
                    tabName = "covid_case_count",
                    icon = icon("chart-bar")
                ),
                bs4Dash::menuItem(
                    "Daily Cases",
                    tabName = "daily_cases",
                    icon = icon("chart-line")
                )
            )
        ),
        bs4Dash::dashboardBody(
            bs4Dash::tabItems(
                bs4Dash::tabItem(
                    tabName = "covid_case_count",
                    fillRow(
                        flex = c(1, 3),
                        bs4Dash::box(
                            title = "Select Regions",
                            mod_active_cases_table_ui("case_table"),
                            width = 12
                        ),
                        bs4Dash::tabBox(
                            tabPanel(
                                title = "Active Covid-19 Cases",
                                mod_switch_act_plot_pop_ui("per_cap"),
                                mod_active_cases_plot_ui("active_plot"),
                                width = 12
                            ),
                            tabPanel(
                                title = "Covid-19 Cases per Day",
                                mod_daily_cases_point_ui("daily_points"),
                                mod_daily_cases_plot_ui("daily_plot"),
                                width = 12
                            ),
                            tabPanel(
                                title = "Testing_Text",
                                #tableOutput("printtable"),
                                verbatimTextOutput("printtext")
                            ),
                            width = 12
                        )
                    )
                )
            )
        )
    )

    server <- function(input, output, session) {

        current_selection <- mod_active_cases_table_server(
            "case_table",
            merged_active_cases
        )

        #selected_table <- reactive(
        #    subset(merged_daily_table, region %in% as.vector(merged_active_cases$region[current_selection()]))
        #)

        #output$printtable <- renderTable({
        #    selected_table()
        #})

        case_field <- mod_switch_act_plot_pop_server("per_cap")

        output$printtext <- renderText({case_field()})

        mod_active_cases_plot_server(
            "active_plot",
            merged_active_cases,
            current_selection,
            case_field
        )

        points_visible <- mod_daily_cases_point_server(
            "daily_points"
        )

        mod_daily_cases_plot_server(
            "daily_plot",
            merged_daily_table,
            current_selection,
            merged_active_cases,
            points_visible
        )

    }

    shinyApp(ui, server)
}
