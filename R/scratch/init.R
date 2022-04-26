pacman::p_load(
    "salesforcer", 
    "tidyverse", 
    "lubridate"
)

conf <- config::get()

salesforcer::sf_auth(cache = FALSE)

contact_description <- salesforcer::sf_describe_objects("Contact")

contact_description[[1]]

contact_fields <- salesforcer::sf_describe_object_fields("Contact")

contact_fields_trimmed <- contact_fields |>
    select(
        label, 
        name, 
        soapType, 
        picklistValues
    )

contact_fields |> filter(name |> str_detect("Record"))
contact_fields_trimmed |> filter(name  |> str_detect("Type"))

contact_record_types <- sf_query(
    "SELECT
        Id, 
        RecordTypeId, 
        RecordType.Name
    FROM 
        Contact
    "
)

contact_record_types |>
    count(RecordTypeId, RecordType.Name)

contact_record_types |> count(npe01__Type_of_Account__c, RecordTypeId)

contact_record_types_meta <- sf_read_metadata(
    metadata_type = "RecordType", 
    object_names = "Contact"
)

system_record_types_fields <- sf_describe_object_fields("RecordType") |>
    select(
        label, 
        name
    )

system_record_types_fields

record_type_data <- sf_query(
    glue::glue(
        "
        SELECT
            {paste0(system_record_types_fields$name, collapse = ', ')}
        FROM 
            RecordType
        "
    )
)

record_type_data |>
    write_z("record-types.rds")

contact_record_types <- sf_query(
    "SELECT
        Id,
        Name
    FROM
        RecordType"
)

contact_record_types

# Participants

# Get RecordTypes

pl_participant <- sf_get_page_layout("Contact", record_type_id = "01250000000MVMMAA4", refresh_cache = TRUE)



# Placements

desc_placement <- sf_describe_objects()

## Name of placement object (TSC_Placement__c)
object_list <- sf_list_objects()
object_list
names(object_list)
object_list_sobjects <- object_list$sobjects
object_example <- object_list_sobjects[[1]]
str(object_example)

object_list_table <- object_list_sobjects |>
    transpose() |>
    as_tibble() |>
    select(-urls) |>
    unnest(cols = everything())

object_list_table

object_list_table  |>
    filter(name |> str_detect("Job")) |>
    select(name)

object_list_table |>
    select(name) |>
    View()



## Get field names

### Can't get fields, though the name is correct
fields_placement <- sf_describe_object_fields("TSC_Placement__c")

test_placement_query <- sf_query(
    "
    SELECT
        Name
    FROM 
        TSC_Placement__c
    "
)

## Get the page layout 
## Pull the data 

## Action Items

## - Inquire wrt to permissions on TSC Placement records

# Alumni Survey

## Get Object Name
object_list_table |>
    filter(name |> str_detect("urvey"))
## Get Field Names
fields_survey <- sf_describe_object_fields("Alumni_Survey__c")
## Get Page Layout
## Pull the data 