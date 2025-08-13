
# WIP ####

#' UNSTABLE Fill missing soil horizon intervals by linear interpolation
#'
#' This function fills missing depth intervals within a specified range by
#' interpolating `TOCstock` values using linear approximation between adjacent horizons.
#'
#' @param data A data frame containing soil horizon data.
#' @param depth_from Numeric. Lower bound of the depth interval to check (in meters).
#' @param depth_to Numeric. Upper bound of the depth interval to check (in meters).
#' @param depth_top_var Character. Name of the column containing the top depth of horizons (in meters).
#' @param depth_bottom_var Character. Name of the column containing the bottom depth of horizons (in meters).
#' @param stock_var Character. Name of the column containing the total stock per horizon (e.g., TOCstock in T/ha).
#' @param group_vars Character vector. Names of the grouping variables (e.g., site_id, Profile) used to group profiles.
#' @param min_thickness Numeric. Minimum gap (in meters) to consider as a missing horizon. Defaults to 0.01 (1 cm).
#'
#' @return A data frame with interpolated horizon layers added where gaps were detected within the target depth range.
#'         New interpolated layers will have estimated `TOCstock` values based on the average stock density of adjacent layers.
#'
#' @examples
#' df_filled <- fill_missing_layers(df, depth_from = 0, depth_to = 0.3,
#'                                  depth_top_var = "Depth_top",
#'                                  depth_bottom_var = "Depth_bottom",
#'                                  stock_var = "TOCstock",
#'                                  group_vars = c("site_id", "Profile"))
#'@import tidyverse
#' @export
fill_missing_layers <- function(data,
                                depth_from = 0,
                                depth_to = 0.3,
                                depth_top_var = "Depth_top",
                                depth_bottom_var = "Depth_bottom",
                                stock_var = "TOCstock",
                                group_vars = c("site_id", "Profile"),
                                min_thickness = 0.01) {

  # NSE symbols for tidy eval
  top_sym <- sym(depth_top_var)
  bottom_sym <- sym(depth_bottom_var)
  stock_sym <- sym(stock_var)
  group_syms <- syms(group_vars)

  data %>%
    rename(Depth_top = !!top_sym, Depth_bottom = !!bottom_sym, TOCstock = !!stock_sym) %>%
    group_by(!!!group_syms) %>%
    arrange(Depth_top) %>%
    group_modify(function(df, keys) {
      # # Add interpolated = FALSE to original data
      df <- df %>% mutate(interpolated = FALSE)

      # Find gaps
      expected_tops <- df$Depth_bottom[-nrow(df)]
      actual_tops <- df$Depth_top[-1]
      gaps <- which(abs(expected_tops - actual_tops) > min_thickness)

      if (length(gaps) == 0) return(df)

      interpolated_layers <- map_dfr(gaps, function(i) {
        top <- df$Depth_bottom[i]
        bottom <- df$Depth_top[i + 1]
        thickness <- bottom - top

        if (top >= depth_to || bottom <= depth_from) return(NULL)  # skip outside range

        # Interpolate TOCstock by midpoint average
        stock_above <- df$TOCstock[i]
        stock_below <- df$TOCstock[i + 1]
        avg_stock_density <- (stock_above / (df$Depth_bottom[i] - df$Depth_top[i]) +
                                stock_below / (df$Depth_bottom[i + 1] - df$Depth_top[i + 1])) / 2

        interpolated_TOCstock <- avg_stock_density * thickness

        tibble(
          Depth_top = top,
          Depth_bottom = bottom,
          TOCstock = interpolated_TOCstock,
          interpolated = TRUE
        )
      })

      # Combine and sort
      bind_rows(df, interpolated_layers) %>%
        arrange(Depth_top)
    }) %>%
    ungroup()
}








#' @title UNSTABLE Stock calculation from soil profile
#' @description Function to compute weighted TOCstock for a given depth limit (in meters)
#' @param data Soil profile dataset including at least a Depth top and bottom, a stock variable
#' @param depth_from Upper boundary in m. Defualt 0
#' @param depth_to Lower boundary in m. Default .3
#' @param stock_var Stock variable name as character string
#' @import tidyverse
#' @importFrom rlang syms
#' @export
compute_TOCstock <- function(data,
                             depth_from=0,
                             depth_to=.3,
                             depth_top_var="Depth_top",
                             depth_bottom_var="Depth_bottom",
                             stock_var="TOCstock",
                             grouping_vars=c("site_id","Profile")
                             ) {

  grouping_vars=syms(grouping_vars)
  data %>%
    rename(stock=.data[[stock_var]],
           Depth_top=.data[[depth_top_var]],
           Depth_bottom=.data[[depth_bottom_var]])%>%
    mutate(
      thickness = Depth_bottom - Depth_top
    )%>%
    mutate(
      # Calculate horizon-thickness and overlap
      overlap = pmin(Depth_bottom, depth_to) - pmax(Depth_top, depth_from),
      overlap = pmax(overlap, 0),  # Set negative overlaps to 0
      weight = overlap / thickness,
      weighted_stock = stock * weight
    )%>%
    group_by(!!!grouping_vars) %>%
    summarise(TOCstock = mean(weighted_stock,na.rm=T)* #weighted avg stock (g/cm³) or T ha-1 cm-1
                (depth_to-depth_from)* # times depth increment (in m)
                100, # m->cm; Unit is T ha-1 for depth increment
              .groups = "drop") %>%
    mutate(depth_interval = paste0(depth_from, "_", depth_to, "m"))
}
