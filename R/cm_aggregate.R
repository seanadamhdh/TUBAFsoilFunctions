



#' cm-wise aggregation of soil data
#'
#' @description Dataset containing sampling depth (upper+lower) and numeric values can be aggregated to equal depth increments.
#' Depths and `res_out` should be stated in meters.
#' @param dataset An R tibble or data.frame
#' @param depth_top_col Upper sampling depth column name
#' @param depth_bottom_col Lower sampling depth column name
#' @param aggregate_list Columns to be aggregated. Can be character vector or string. Selected column must be numeric
#' @param group_list Optional grouping variables (e.g., sites, sampling points, soil cores etc.).
#' Default set to o3 corresponding to output upper depth (no additional grouping).
#' @param res_out Aggregate depth increments that are to be returned. Should be same unit as depth_bottom_col and depth_top_col.
#' @param add_funs List of additional function arguments like sd or n that can be passed to summarise. This will change output for aggregate vars to ..._mean, ..._sd, etc.
#' @import tidyverse
#' @export
#'
cm_aggregate=function(dataset,depth_top_col,depth_bottom_col,aggregate_list,group_list="o3",res_out=.1,add_funs=NA){

  if(is.na(add_funs)){
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
      o3 = floor((o2+1E-10) / res_out) * res_out,  # Calculate lower bound of 10cm intervals for o2 cm->dm # add /subtract very small number to avoid rounding error
      u3 = ceiling((u2-1E-10) / res_out) * res_out  # Calculate upper bound of 10cm intervals for u2 # add /subtract very small number to avoid rounding error
    ) %>%
    group_by(across(all_of(c("o3","u3",group_list))))%>%
    summarize(
      across(.cols=all_of(aggregate_list),
             .fns =  ~mean(.,na.rm=T)  # Summarize the 'value' column (e.g., sum)
      ),
      .groups = "drop"
    )->df_aggregated
  }else{

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
        o3 = floor((o2+1E-10) / res_out) * res_out,  # Calculate lower bound of 10cm intervals for o2 # add /subtract very small number to avoid rounding error
        u3 = ceiling((u2-1E-10) / res_out) * res_out  # Calculate upper bound of 10cm intervals for u2 # add /subtract very small number to avoid rounding error
      ) %>%
      group_by(across(all_of(c("o3","u3",group_list))))%>%
      summarize(
        across(.cols=all_of(aggregate_list),
               .fns =  c(list(mean=~mean(.,na.rm=T)),add_funs)  # Summarize the 'value' column (e.g., sum)
        ),
        .groups = "drop"
      )->df_aggregated

}
  # Display the aggregated result
  return(df_aggregated)


}









