pacman::p_load(
    "salesforcer", 
    "tidyverse", 
    "lubridate"
)

record_types <- read_z("record-types.rds")

contact_record_types <- record_types |>
    filter(SobjectType == "Contact")
contact_record_types

contact_data <- sf_select_all("Contact")

contact_fields <- sf_describe_object_fields("Contact")

contact_layout_student <- sf_get_page_layout("Contact", "01250000000MVMMAA4")

contact_fields_student <- contact_layout_student$section_components |> unlist()


contact_data_student <- sf_query(
    glue::glue(
        "SELECT
            {paste(contact_fields_student, collapse = ', ')}
        FROM 
            Contact
        WHERE
            RecordType.DeveloperName = 'TSC_Student_Young_Adult'
        "
    )
)

fetch_student_contacts <- function(force_request = FALSE) {
    
    
}