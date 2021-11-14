on_cols_for_plotting <- function() {
    on_cols_for_plot <- c(
        "Date",
        "Ottawa_Public_Health",
        "Peel_Public_Health",
        "Toronto_Public_Health",
        "Hamilton_Public_Health_Services",
        "Halton_Region_Health_Department",
        "Durham_Region_Health_Department",
        "York_Region_Public_Health_Services")
    return(on_cols_for_plot)
}

ontario_daily_col_names <- function() {

    on_colnames <- c(
        "Date",
        "Algoma_Public_Health_Unit",
        "Brant_County_Health_Unit",
        "Chatham-Kent_Health_Unit",
        "Durham_Region_Health_Department",
        "Eastern_Ontario_Health_Unit",
        "Grey_Bruce_Health_Unit",
        "Haldimand-Norfolk_Health_Unit",
        "Haliburton_Kawartha_Pine_Ridge_District_Health_Unit",
        "Halton_Region_Health_Department",
        "Hamilton_Public_Health_Services",
        "Hastings_and_Prince_Edward_Counties_Health_Unit",
        "Huron_Perth_District_Health_Unit",
        "Kingston_Frontenac_and_Lennox_&_Addington_Public_Health",
        "Lambton_Public_Health",
        "Leeds_Grenville_and_Lanark_District_Health_Unit",
        "Middlesex-London_Health_Unit",
        "Niagara_Region_Public_Health_Department",
        "North_Bay_Parry_Sound_District_Health_Unit",
        "Northwestern_Health_Unit",
        "Ottawa_Public_Health",
        "Peel_Public_Health",
        "Peterborough_Public_Health",
        "Porcupine_Health_Unit",
        "Region_of_Waterloo,_Public_Health",
        "Renfrew_County_and_District_Health_Unit",
        "Simcoe_Muskoka_District_Health_Unit",
        "Southwestern_Public_Health",
        "Sudbury_&_District_Health_Unit",
        "Thunder_Bay_District_Health_Unit",
        "Timiskaming_Health_Unit",
        "Toronto_Public_Health",
        "Wellington-Dufferin-Guelph_Public_Health",
        "Windsor-Essex_County_Health_Unit",
        "York_Region_Public_Health_Services",
        "Total")
    return(on_colnames)
}

create_list_of_regions <- function(merged_active_cases) {
    list_of_regions <- merged_active_cases[[region]]
    return(list_of_regions)
}