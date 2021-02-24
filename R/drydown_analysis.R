#' @title Quality control for drying soil specimens
#'
#' @description Take an input data file and calculate water loss for each
#'   specimen, then plot the results. A linear rate of water loss must be
#'   assumed because only 2 data points are collected; however strictly speaking
#'   the water loss usually is linear above the shrinkage limit and then becomes
#'   curvilinear as the soil dries further.
#'
#' @param path path to raw data file
#'
#' @return 3-item list of class `"soilmesh_drydown"`. All water content references are on a gravimetric basis
#'
#' 1. `water_losses` Tibble containing drying time, total water loss, and rate of water loss per hour
#' 2. `water_loss_summary` Tibble containing average water loss data across all 3 cylinders of the same soil
#' 3. `drydown_plot`ggplot showing drydown information for each soil
#'
#' @export
#'
#' @example inst/examples/drydown_analysis_example.R
#'
drydown_analysis <- function(path){

  if(!requireNamespace("colorblindr")){
    stop("This function requires package `colorblindr`. `colorblindr` also requires the development versions of `cowplot` and `colorspace`.
         See instructions at https://github.com/clauswilke/colorblindr")
  }

  if(!requireNamespace("ggrepel")){
    stop("This function requires the `ggrepel` package. Please install it.")
  }

  if(!requireNamespace("ggrepel")){
    stop("This function requires the `scales` package. Please install it.")
  }

  if(!requireNamespace("asi468")){
    stop("This function requires the `asi468` package. Please install it.")
  }

  #browser()


  # read raw file
  # specify that cells containing a single dash should be treated as NA values
  # then construct a date time object from the date and time

  raw_data <- suppressMessages(
    readr::read_csv(path, na = "-") %>%
    dplyr::mutate(date_time = lubridate::as_datetime(paste(.data$date, .data$time, .data$AM_PM),
                                                     tz = "America/New_York"))
  )


  # find tin tare set from data file

  tin_tare_date <- unique(as.character(raw_data$tin_tare_set))

  # set test date as a variable

  data_collection_date <- min(lubridate::as_date(raw_data$date))


  # compute the time changes for each cylinder

  time_changes  <- raw_data %>%
    dplyr::select(-c(.data$time, .data$tin_tare_set, .data$tin_number,
                     .data$tin_w_wet_sample, .data$tin_w_OD_sample))%>%
    tidyr::pivot_wider(names_from = .data$time_type, values_from = .data$date_time) %>%
    dplyr::mutate(drydown_time = lubridate::as.duration(.data$test_time - .data$lamp_on)) %>%
    dplyr::select(.data$date, .data$soil_ID, .data$cylinder_ID, .data$drydown_time)


  # compute water content loss for each cylinder

  water_losses <- suppressMessages(
    raw_data %>%
    dplyr::left_join(asi468::tin_tares[[tin_tare_date]]) %>%
    soiltestr::add_w() %>%
    dplyr::select(.data$date, .data$soil_ID, .data$cylinder_ID, .data$time_type, .data$water_content) %>%
    tidyr::pivot_wider(names_from = .data$time_type,
                values_from = .data$water_content) %>%
    dplyr::mutate(w_loss_total = .data$lamp_on - .data$test_time) %>%
    dplyr::left_join(time_changes) %>%
    dplyr::mutate(w_loss_per_hr = .data$w_loss_total / lubridate::time_length(.data$drydown_time, unit = "hours")) %>%
    dplyr::select(.data$date, .data$soil_ID, .data$cylinder_ID, .data$drydown_time,
                  .data$w_loss_total, .data$w_loss_per_hr) %>%
    dplyr::ungroup()
  )



  # compute a summary for each soil

    water_losses_summary <- suppressMessages(
      water_losses %>%
    dplyr::group_by(.data$date, .data$soil_ID) %>%
    dplyr::summarise(avg_w_loss_total = mean(.data$w_loss_total, na.rm = T),
                     avg_w_loss_per_hr = mean(.data$w_loss_per_hr, na.rm = T)) %>%
    dplyr::ungroup()
    )



# plot the two sets of water contents with time ---------------------------

# First need to make some new data frames

    # I am having trouble mapping the location of the text as Inf because
    # the time is a POSIXct object which throws a conversion error

    # assign 2 new variables: one for the max overall water content and one for the max
    # overall time

    max_w <- suppressMessages(raw_data %>%
      dplyr::left_join(asi468::tin_tares[[tin_tare_date]]) %>%
      soiltestr::add_w() %>%
      .$water_content %>%
      max(na.rm = T)
    )

    max_end_time <- max(raw_data$date_time)

    # construct the data frame containing the stuff to
    # be plotted as a text annotation using the max values computed above
    # plus the unique values of the soil IDs

    plot_annotation_data <- suppressMessages(
      tibble::tibble(
      soil_ID = unique(raw_data$soil_ID),
      date_time = max_end_time,
      water_content = max_w
    ) %>%
      dplyr::left_join(water_losses_summary) %>%
      dplyr::mutate(plot_text = paste0("Avg loss\n", round(100*.data$avg_w_loss_per_hr, 2), "% /hr"))
    # could construct a plotmath expression here to use the delta symbol, but not important right now
)
    # make a data frame containing the water contents, for plotting purposes

    plotting_data <- suppressMessages(raw_data %>%
      dplyr::left_join(asi468::tin_tares[[tin_tare_date]]) %>%
      soiltestr::add_w()
    )

# construct the plot

    # w_cont_labs_data <- plotting_data %>%
    #   dplyr::filter(.data$time_type == "lamp_on")

    drydown_plot <-  plotting_data %>%
      ggplot2::ggplot(ggplot2::aes(x= .data$date_time, y = .data$water_content,
                                   group = .data$cylinder_ID, color =.data$soil_ID))+
      ggplot2::geom_point()+
      ggplot2::geom_line(linetype = 'solid', size = 0.25)+
      cowplot::theme_cowplot()+
      cowplot::background_grid(color.major = 'grey95',
                               color.minor = 'grey95',
                               size.major = 0.25, size.minor = 0.25)+
      colorblindr::scale_color_OkabeIto()+
      ggplot2:: facet_wrap(~soil_ID)+
      ggplot2::geom_text(data = plot_annotation_data,
                         inherit.aes = F,
                         mapping = ggplot2::aes(
                           x= .data$date_time,
                           y = .data$water_content,
                           label = .data$plot_text),
                         vjust= "inward",
                         hjust = "inward")+
     # ggrepel::geom_text_repel(#data = w_cont_labs_data,
     #                    ggplot2::aes(label = round(100*.data$water_content, 1)),
     #                    color = 'grey25', group = NA, size = 3,
                         #nudge_x = lubridate::dminutes(10),
     #                    hjust = 'inward', vjust= 'inward')+
      ggplot2::scale_x_datetime("Clock time", date_labels = "%l:%M")+
      ggplot2::scale_y_continuous(bquote("Water content, % g g"^1),
                                  labels = scales::label_percent(accuracy = 1, suffix = ""))+
      ggplot2::labs(title = 'Soil water loss during drydown (20 mm depth)',
                    subtitle = paste0("Data collected ", data_collection_date))+
      ggplot2::theme(legend.position = 'none',
                     strip.background = ggplot2::element_rect(fill = 'grey90'))


    # return a 3-item list: the water loss for each cylinder,
    # the average values for each soil,
    # and the faceted plot

    return_val <- list(
      water_losses = water_losses,
      water_losses_summary = water_losses_summary,
      drydown_plot = drydown_plot
      )

    # assign a new class called `soilmesh_drydown` to this object;
    # I may create specific methods which opertae on this class
    # at a later date. Just learning to use the S3 system.

    class(return_val) <- "soilmesh_drydown"

  return(return_val)

    }

