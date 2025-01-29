


#' @title PriEco8800 Flux calculator
#' @description flux calculation for PriEco8800 incubator. This function might be unstable. Use at own risk.
#' @note optimise potentially: Function for individual meas, in main function, group by run,round,sample; summarise
#' @param dataset Flux measurement data for slope calculation as loaded by load_pri8800_data.
#' @param V_ch_list Total system volume (accumulation volume) of each incubation vessel. If NA defaults to 1. cubic meter
#' @param A_ch Default base area - For compatibility with field chambers. If NA defaults to 1. square meter
#' @param wt_list List of soil sample weights in each vessel. grams
#' @import tidyverse
#' @import progress
#' @export
calc_flux_manual=function(dataset,V_ch_list=c(NA),A_ch=NA,wt_list=c(NA)){
  results=c()
  measurements=unique(paste(dataset$run,dataset$round,dataset$sample,sep="-"))
  pb=progress_bar$new(total=length(measurements),format="Manual flux calculation: [:bar] :percent eta: :eta",force = T)
  for (i in measurements){
    pb$tick()
    # run-round-sample
    measurement_i=c(str_split_fixed(i,"-",3))

    flag=""
    V_ch=V_ch_list[as.numeric(measurement_i[3])]
    if(is.na(V_ch)){
      flag=flag=paste("No V_ch provided, setting to 1")
      V_ch=1
    }
    wt=wt_list[as.numeric(measurement_i[3])]
    if(is.na(wt)){
      wt=1
    }
    #A_ch is constant
    if(is.na(A_ch)){
      flag=paste(flag,"No A_ch provided, setting to 1")
      A_ch=1
    }

    individual_meas=filter(dataset,run==measurement_i[1],round==measurement_i[2],sample==measurement_i[3])%>%
      transmute(run=run,
                round=round,
                sample=sample,
                time=time,
                sens_tempK=sensor_temperature+273.15, # °C to K
                temp=temperature, #°C
                #water_temp=water_temperature, #°C
                presPa=PRES*1E3,#kPa to Pa
                CO2=CO2, #ppm,
                sens_H2O=sensor_humidity #ppm
      )%>%
      reframe(
        across(where(is.numeric),
               function(x){
                 new=approx(x=time,y=x,n=max(time)+1) # set equidistant 1s intervals
                 return(new$y)
               }))
    individual_meas=mutate(individual_meas,
                           CO2_corr=CO2*presPa/sens_tempK)
    individual_meas%>%group_by(run,round,sample)%>%
      summarise(slope=lm(CO2~time)[["coefficients"]][[2]],
                R2=summary(lm(CO2~time))$r.squared,
                mean_sens_tempK=mean(sens_tempK),
                mean_pres=mean(presPa),
                mean_temp=mean(temp),
                #mean_water_temp=mean(water_temp),
                mean_sens_H2O=mean(sens_H2O),
                slope_corr=lm(CO2_corr~time)[["coefficients"]][[2]],
                R2_corr=summary(lm(CO2~time))$r.squared,
                .groups = "keep" # get rid of warnings
                )%>%
      mutate(
        F_CO2=slope*V_ch*mean_pres/8.314/A_ch/mean_sens_tempK,
        F_CO2_corr=slope_corr*V_ch/8.134/A_ch,
        F_CO2_wt=12*slope*V_ch*mean_pres/8.314/wt/mean_sens_tempK*24*3600,
        F_CO2_wt_corr=12*slope_corr*V_ch/8.314/wt*24*3600,
        flag=flag)->results_i
    results=rbind(results,results_i)
    #print(results_i) #debug
  }
  return(results)
}


#' @title load Pri8800 data
#' @description load different files created by Pri8800 (both inidvidual slopes and final output files).
#' @param folder Path to folder containing measurement files.
#' @param contains_folders Boolean. Recursive read in or not.
#' @param calculate_flux_manual Boolean. Should flux be re-calculated from slope data? This might be slow.
#' @param V_ch_list Total system volume (accumulation volume) of each incubation vessel. If NA defaults to 1. cubic meter
#' @param A_ch Default base area - For compatibility with field chambers. If NA defaults to 1. square meter
#' @param wt_list List of soil sample weights in each vessel. grams
#'
#' @import tidyverse
#' @import progress
#' @export
load_pri8800_data=function(
    folder,
    contains_folders=T,
    calculate_flux_manual=F,
    V_ch_list=c(NA),
    A_ch=NA,
    wt_list=c(NA)
){
  # load slope data
  {
    slope_file_list=list.files(folder,full.names = T,recursive = contains_folders)%>%str_subset("_Flux",negate = T)
    pb=progress_bar$new(total=length(slope_file_list),format="Reading slope data: [:bar] :percent eta: :eta",force = T)
    Meas_data=c()
    for (i in slope_file_list){
      Meas_data <- bind_rows(Meas_data,read_csv(i,progress = F,show_col_types = F))
      pb$tick()
    }
  }


  # format slope data || SLOW AF... if possible, optimize
  {
    #pb=progress_bar$new(
    #  total=length(shift.column(Meas_data,"round",newNames = "next_round",len=1,up = F)%>%
    #                 mutate(delta_round=round-next_round)%>%pull(delta_round)),
    #  format="Formatting slope data: [:bar] :percent eta: :eta")


    #shift.column(Meas_data,"round",newNames = "next_round",len=1,up = F)%>%
    #  mutate(delta_round=round-next_round)%>%
    #  mutate(if_else(delta_round<0,run=1))


    print("formatting slope")
    Meas_data=shift.column(Meas_data,"round",newNames = "next_round",len=1,up = F)%>%
      mutate(delta_round=round-next_round)%>%
      mutate(run=cumsum(delta_round<0))
    #runi=1
    #run=c(1)
    #for (i  in shift.column(Meas_data,"round",newNames = "next_round",len=1,up = F)%>%mutate(delta_round=round-next_round)%>%pull(delta_round)){
    #  if (i <0){runi=runi+1}
    #  run=c(run,runi)
    #  pb$tick()
    #}
    #Meas_data$run=run
    Meas_data=(Meas_data%>%group_by(sample,run,round)%>%mutate(time=(datatime-first(datatime))%>%as.numeric(unit="secs")))
    print("done")
  }




  # load flux data
  {
    print("loading flux data")
    flux_file_list=list.files(folder,full.names = T,recursive = contains_folders,pattern="_Flux")
    pb=progress_bar$new(total=length(flux_file_list),format="Formatting flux data: [:bar] :percent eta: :eta",force = T)
    Flux_data=c()
    for (i in flux_file_list){
      Flux_data <- bind_rows(Flux_data,read_csv(i,progress = F,show_col_types = F))
      pb$tick()
    }
    print("done")
  }


  # format flux data
  {
    print("formatting flux data")

    Flux_data=shift.column(Flux_data,"round",newNames = "next_round",len=1,up = F)%>%
      mutate(delta_round=round-next_round)%>%
      mutate(run=cumsum(delta_round<0))

    # pb=progress_bar$new(
    #    total=length(shift.column(Flux_data,"round",newNames = "next_round",len=1,up = F)%>%
    #                   mutate(delta_round=round-next_round)%>%pull(delta_round)),
    #    format="Reading flux data: [:bar] :percent eta: :eta",force = T)
    #  runi=1
    #  run=c(1)
    #  for (i  in shift.column(Flux_data,"round",newNames = "next_round",len=1,up = F)%>%
    #       mutate(delta_round=round-next_round)%>%pull(delta_round)){
    #    if (i <0){runi=runi+1}
    #    run=c(run,runi)
    #    pb$tick()
    # }
    # Flux_data$run=run
    print("done")
  }



  # manual calcualtion of flux
  if(calculate_flux_manual==T){
    print("manual flux calculation")
    Flux_data_manual=calc_flux_manual(Meas_data,V_ch_list=V_ch_list,A_ch=A_ch,wt_list=wt_list)

    print("Merging flux and slope data...")

    Meas_data_consolidated=Meas_data%>%group_by(run,round,sample)%>%summarise(start=first(datatime),end=last(datatime),
                                                                              aq_temp=mean(water_temperature),
                                                                              sens_temp=mean(sensor_temperature),
                                                                              air_temp=mean(temperature))
    Flux_data_all=left_join(Flux_data,
                            Flux_data_manual,
                            by=c("run","round","sample"))%>%
      left_join(Meas_data_consolidated,
                by=c("run","round","sample")
      )
    print("done")


    # output
    return(list("Flux_data_all"=Flux_data_all,"Flux_data"=Flux_data,"Meas_data"=Meas_data,"Flux_data_manual"=Flux_data_manual))

  }else{

    print("Merging flux and slope data...")
    Meas_data_consolidated=Meas_data%>%group_by(run,round,sample)%>%summarise(start=first(datatime),end=last(datatime),
                                                                              aq_temp=mean(water_temperature),
                                                                              sens_temp=mean(sensor_temperature),
                                                                              air_temp=mean(temperature))
    Flux_data_all=left_join(Flux_data,
                            Meas_data_consolidated,
                            by=c("run","round","sample")
    )

    print("done")

    # output
    return(list("Flux_data_all"=Flux_data_all,"Flux_data"=Flux_data,"Meas_data"=Meas_data))
  }

}





















