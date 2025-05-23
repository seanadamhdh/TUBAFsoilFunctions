



#' cm-wise aggregation of soil data
#'
#' Dataset containing sampling depth (upper+lower) and numeric values can be aggregated to equal depth increments.
#'
#' @param dataset An R tibble or data.frame
#' @param depth_top_col Upper sampling depth column name
#' @param depth_bottom_col Lower sampling depth column name
#' @param aggregate_list Columns to be aggregated. Can be character vector or string. Selected column must be numeric
#' @param group_list Optional grouping variables (e.g., sites, sampling points, soil cores etc.)
#' @param res_out Aggregate depth increments in cm
#' @import tidyverse
#' @export
#'
cm_aggregate=function(dataset,depth_top_col,depth_bottom_col,aggregate_list,group_list,res_out=.1){

  res_out=res_out*100

  # expand data to 1 cm increments
  dataset %>%
    rowwise() %>%
    mutate(
      cm_values = list(seq(.data[[depth_top_col]] * 100, .data[[depth_bottom_col]] * 100, by = 1))  # Generate 1 cm increments in cm
    ) %>%
    unnest(cols = cm_values) %>%  # Expand the list into multiple rows
    mutate(
      o2 = cm_values / 100,        # Convert back to meters for the lower bound
      u2 = (cm_values + 1) / 100   # Convert to meters for the upper bound (next cm)
    ) %>%
    ungroup() %>%

    # Create depth intervals and aggregate, handling negative depths
    mutate(
      o3 = floor(o2 * res_out) / res_out,  # Calculate lower bound of 10cm intervals for o2 cm->dm
      u3 = ceiling(u2 * res_out) / res_out  # Calculate upper bound of 10cm intervals for u2
    ) %>%
    group_by(across(all_of(c("o3","u3",group_list))))%>%
    summarize(
      across(.cols=all_of(aggregate_list),
             .fns =  ~mean(.,na.rm=T)  # Summarize the 'value' column (e.g., sum)
      ),
      .groups = "drop"
    )->df_aggregated

  # Display the aggregated result
  return(df_aggregated)


}









