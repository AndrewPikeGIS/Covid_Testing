
create_list_of_regions <- function(merged_active_cases) {
    list_of_regions <- merged_active_cases[["region"]]
    return(list_of_regions)
}

ab_regions <- c(
    "CALGARY ZONE",
    "EDMONTON ZONE",
    "CENTRAL ZONE",
    "NORTH ZONE",
    "SOUTH ZONE")
