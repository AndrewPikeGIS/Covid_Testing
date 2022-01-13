build_disclaimer <- function() {
    disclaimer <- paste0(
        "This app is meant as a small showcase of my Shiny dashboard ",
        "development and not an official dashboard for case tracking. ",
        "Due to local government cutbacks, PCR testing has been scaled ",
        "back across many of the regions where data is pulled from. ",
        "As a resuly, case numbers should be assumed to be higher than ",
        "reported."
    )

    return(disclaimer)
}