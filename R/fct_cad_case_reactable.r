reactable_theme <- function() {
    options(reactable.theme = reactableTheme(
    color = "hsl(233, 9%, 87%)",
    backgroundColor = "hsl(233, 9%, 19%)",
    borderColor = "hsl(233, 9%, 22%)",
    stripedColor = "hsl(233, 12%, 22%)",
    highlightColor = "hsl(233, 12%, 24%)",
    inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
    selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
    pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
    pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
    ))
}

filter_for_active_reactable <- function(input_df) {
    output_df <- input_df %>%
        select(
            c("prov",
            "region"))
    return(output_df)
}

build_active_case_reactable <- function(input_df) {
    input_df %>%
        reactable::reactable(
            pagination = TRUE,
            filterable = TRUE,
            highlight = TRUE,
            selection = "multiple",
            onClick = "select",
            compact = TRUE,
            style = list(
                fontFamily = "Work Sans, sans-serif",
                fontSize = "12px",
                color = "black"
            ),
            defaultColDef =  reactable::colDef(
                align = "center"
            )
        )
}