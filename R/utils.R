write_z <- function(x, path) {
    readr::write_rds(x, glue::glue("Z:/FSC - NPower/data-2022/{path}"))
}

read_z <- function(path) {
    readr::read_rds(glue::glue("Z:/FSC - NPower/data-2022/{path}"))
}

z_exists <- function(path) {
    fs::file_exists(glue::glue("Z:/FSC - NPower/data-2022/{path}"))
}

fetch_records <- function(object, force_request = FALSE, pick_record_types = TRUE, record_type_name = NULL, browse = FALSE, drop_dev_fields = TRUE, add_fields = NULL) {
    if(browse) browser()
    cache_path <- glue::glue("records/{object}.rds")

    if(!force_request & z_exists(cache_path)) {
        return(read_z(cache_path))
    }


    dat_fields <- fetch_fields(object) |>
        dplyr::filter(!(name %in% c("Address", "MailingAddress", "OtherAddress", "Latitude", "Longitude")))

    if(is.null(record_type_name) & pick_record_types) {
        record_types <- fetch_record_types()
        object_record_types <- record_types |> 
            dplyr::filter(SobjectType == object) |>
            dplyr::arrange(Name)
        object_record_type_names <- object_record_types$DeveloperName
        prompt_options <- paste(seq_along(object_record_type_names), object_record_type_names)
        cat(
            c(
                glue::glue("{object} record types\n{strrep('=', nchar(object) + 13)}"),
                prompt_options
            ), 
            sep = "\n"
        )
        record_type_selection_input <- readline(
            prompt = "Enter the numbers for the record types you would like to select, separated by spaces.\nTo download records from all record types, enter '0'\n"
        )

        record_type_selection_index <- as.numeric(record_type_selection_input |> stringr::str_extract_all("\\d+", simplify = TRUE))

        if(record_type_selection_index == 0) {
            where_clause <- ""
        } else {
            record_type_selection_names <- object_record_type_names[record_type_selection_index]
            where_conditions <- 
                glue::glue(
                    "(RecordType.DeveloperName = '{record_type_selection_names}')"
                ) |>
                paste(
                    collapse = " OR\n"
                )

            where_clause <- glue::glue("WHERE\n\t{where_conditions}")

            if(drop_dev_fields) {
                layout_fields <- object_record_types |>
                    dplyr::slice(record_type_selection_index) |>
                    dplyr::pull(Id) |>
                    purrr::map(
                        function(x) {
                            layout <- fetch_page_layout(object, x)
                            fields <- layout$section_components |> unlist()
                        } 
                    ) |>
                    unlist() |>
                    unique()
                
                dat_fields <- dat_fields |> dplyr::filter(name %in% c("Id", layout_fields))
            }
        }
    } else {
        where_clause <- ""
    }

    
    soql_fields <- paste0(c(add_fields, dat_fields$name), collapse = ",")

    dat_records <- salesforcer::sf_query(
        glue::glue(
            "SELECT
                {soql_fields}
            FROM 
                {object}
            {where_clause}
            "
        )
    )

    write_z(dat_records, cache_path)

    return(dat_records)


}

fetch_record_types <- function() {
    fetch_records("RecordType", pick_record_types = FALSE)
}

fetch_fields <- function(object, force_request = FALSE) {
    cache_path <- glue::glue("fields/{object}.rds")
    
    if(!force_request & z_exists(cache_path)) {
        return(read_z(cache_path))
    }

    dat_fields <- salesforcer::sf_describe_object_fields(object)

    write_z(dat_fields, cache_path)

    return(dat_fields)
}


#' Get the page layout for an object. specify its record type id if necessary
#'
#' @param object 
#' @param record_type_id 
#' @param refresh_cache 
#'
#' @return
#' @export
#'
#' @examples
fetch_page_layout <- function(object, record_type_id, force_request = FALSE) {

    cache_path <- glue::glue("layouts/{object}-{record_type_id}.rds")

    if(!force_request & z_exists(cache_path)) {
        return(read_z(cache_path))
    }

    url <- salesforcer:::make_rest_objects_url(glue::glue("{object}/describe/layouts/{record_type_id}"))
    raw <- salesforcer::rGET(url) |> httr::content()
    
    res <- raw$detailLayoutSections |> 
        map(.clean_detail_layout_section) |> 
        transpose() |> 
        as_tibble() |> 
        mutate(section_id = unlist(section_id), section_title = unlist(section_title))

    write_z(res, cache_path)

    res
  
}

.pull_layout_component_details <- function(d) {
  
  fields_table <- d$layoutRows |> 
    map(
      ~ .x$layoutItems |> 
        map(
          ~ .x$layoutComponents |> 
            map(
              ~ list(name = .x$details$name, label = .x$details$label)
            )
        )
    ) |> 
    flatten() |> 
    flatten() |> 
    transpose() |> 
    map(unlist) |> 
    as_tibble()
  
  res <- fields_table$name |> set_names(fields_table$label)
  
  res
}

.clean_detail_layout_section <- function(d) {
  
  res <- list(
    section_id = d$layoutSectionId, 
    section_title = d$heading, 
    section_components = .pull_layout_component_details(d)
  ) 
  
  res  
}
