# This is some interesting code; I ended up not using it because I wrote a
# function that is much shorter and elegant.....however I could envision this
# strategy being useful some other time, so I am saving it for a reference


# to allow for the possibility of multiple dates and multiple tin tare sets (although only one set of tins can be used on a given day of data collection)
# this has to be done with two separate lists: one of the actual data,
# and the other containing the tin tare lookups, which are uniquely chosen
# based on the contents of the data files.

# first find the set of tin tares used on each date and assign as a list

test_time_water_content_tin_tare_dates <- test_time_water_content_data %>%
  purrr::map(~unique(.$tin_tare_set))

# now use that list to pick out the correct set of tin tares from the
# tin tares data object, which is itself a list and therefore allows
# indexing by a character string corresponding to a single list element
tin_tare_sets <- purrr::map(.x = test_time_water_content_tin_tare_dates,
                            .f= ~asi468::tin_tares[[.]])

# construct a tibble having parallel elements for the water content data
# and the appropriate tin tares
w_cont_args <- list(x = test_time_water_content_data,
                    y = tin_tare_sets)

# join each data frame with its corresponding tin tares, compute the water content, reduce the list of data frames to a single data frame, and select only columns of interest

mesh_water_contents <- pmap(w_cont_args, dplyr::left_join) %>%
  purrr::reduce(rbind) %>%
  soiltestr::add_w() %>%
  dplyr::select(.data$experiment_name, .data$soil_ID, .data$date, .data$cylinder_ID, .data$water_content, .data$comments)

mesh_water_contents
