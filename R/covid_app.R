#' Title
#'
#' @return
#' @export
#' @import shiny
#' @examples

covid_app <- function() {

    ontario_covid_data <- tryCatch({
            load_on_data()
        },
        error = function(cond) {
            return(NA)
        }
    )

    if (is.na(ontario_covid_data)) {
        load("data/on_covid.rda")
    }

    ontario_covid_daily_data <- tryCatch({
            load_daily_on_data()
        },
        error = function(cond) {
            return(NA)
        }
    )

    if (is.na(ontario_covid_daily_data)) {
        load("data/on_daily.rda")
    }

    alberta_covid_data <- tryCatch({
            load_ab_data()
        },
        error = function(cond) {
            return(NA)
        }
    )

    if (is.na(alberta_covid_data)) {
        load("data/ab_covid.rda")
    }

    bc_covid_data <- tryCatch({
            load_bc_data()
        },
        error = function(cond) {
            return(NA)
        }
    )

    if (is.na(bc_covid_data)) {
        load("data/bc_covid.rda")
    }

    sk_covid_data <- tryCatch({
            load_sk_data()
        },
        error = function(cond) {
            return(NA)
        }
    )

    if (is.na(sk_covid_data)) {
        load("data/sk_covid.rda")
    }

    #ontario_covid_data <- load_on_data()

    #ontario_covid_daily_data <- load_daily_on_data()

    #alberta_covid_data <- load_ab_data()

    #bc_covid_data <- load_bc_data()

    #sk_covid_data <- load_sk_data()

    ontario_active_cases <- create_active_on_table(ontario_covid_data)

    ontario_covid_daily_data <- run_loess(ontario_covid_daily_data)

    alberta_active_cases <- create_active_ab_table(alberta_covid_data)

    alberta_daily_covid_data <- create_ab_daily_cases_table(alberta_covid_data)

    alberta_daily_covid_data <- run_loess(alberta_daily_covid_data)

    bc_active_cases <- create_active_bc_table(bc_covid_data)

    bc_covid_daily <- clean_bc_for_daily_plt(bc_covid_data)

    sk_actve_cases <- create_active_sk_table(sk_covid_data)

    sk_covid_daily_data <- clean_sk_table_for_daily(sk_covid_data)

    sk_covid_daily_data <- run_loess(sk_covid_daily_data)

    merged_active_cases <- clean_merge_active_cases_data(
        alberta_active_cases,
        ontario_active_cases,
        bc_active_cases,
        sk_actve_cases) %>%
        add_prov_totals_active()

    merged_daily_table <- merge_daily_covid_tables(
        alberta_daily_covid_data,
        bc_covid_daily,
        ontario_covid_daily_data,
        sk_covid_daily_data
    ) %>%
    add_prov_totals_daily() %>%
    add_color_to_region_name()

    ui <- bs4Dash::dashboardPage(
        fullscreen = TRUE,
        bs4Dash::dashboardHeader(title = "Covid 19 Case Counts"),
        bs4Dash::dashboardSidebar(
            bs4Dash::sidebarMenu(
                bs4Dash::menuItem(
                    "Active and Daily COVID cases",
                    tabName = "covid_case_count",
                    icon = icon("chart-line")
                ),
                bs4Dash::menuItem(
                    "Disclaimer",
                    tabName = "disclaimer",
                    icon = icon("file-alt")
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
                            #tabPanel(
                            #    title = "Testing_Text",
                                #tableOutput("printtable"),
                            #    verbatimTextOutput("printtext")
                            #),
                            width = 12
                        )
                    )
                ),
                bs4Dash::tabItem(
                    tabName = "disclaimer",
                    bs4Dash::box(
                        title = "DISCLAIMER",
                        mod_render_text_ui("disclaimer"),
                        width = 12
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

        output$printtext <- renderText({
            case_field()
        })

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

        mod_render_text_server("disclaimer")

    }

    shinyApp(ui, server)
}
