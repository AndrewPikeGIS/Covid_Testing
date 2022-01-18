build_disclaimer <- function() {
    disclaimer <- paste0(
        "This app is meant as a small showcase of my Shiny dashboard ",
        "development and not an official dashboard for case tracking.\n\n",
        "Due to local government cutbacks, PCR testing has been scaled ",
        "back across many of the regions where data is pulled from.\n",
        "As a resuly, case numbers should be assumed to be higher than ",
        "reported.\n\n",
        "Please see my github for the app repository: \n",
        "https://github.com/AndrewPikeGIS/Covid_Testing"
    )

    return(disclaimer)
}