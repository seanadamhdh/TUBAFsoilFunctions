




#' @title load_SEMACHv3_file
#' @description A function for loading and formatting SEMACHv3 csv measurement files.
#' `r lifecycle::badge("experimental")`
#' @param path A file path to a SEMACHv3 .csv measurement file.
#' @param version SEMACH version for compatability with header read in. Default v3.2 (most recent systems)
#' @returns Formatted raw SEMACHv3 measurement file
#' @import tidyverse
#' @export
load_SEMACHv3_file=function(path,version="v3.2"){

  if(version=="v3.2"){
               #print("ok")
               out=((read_delim(path,delim = ";", escape_double = FALSE, trim_ws = TRUE,show_col_types = FALSE))[-1,])%>%
               transmute(                                                                                                  #channel
                 Timestamp                     = as.POSIXct(Timestamp,format="%d.%m.%Y %H:%M:%S"),                         # 1
                 GPS_longitude                 = as.numeric(`GPS (longitude)`),                                            # 2
                 GPS_latitude                  = as.numeric(`GPS (latitude)`),                                             # 3
                 GPS_altitude                  = as.numeric(`GPS (altitude_m)`),                                           # 4
                 GPS_signal                    = as.logical(`GPS (fix)`),                                                  # 7
                 Temperature_in                = as.numeric(`Temperature intern (degC)`),                                    # 8
                 Humidity_in                   = as.numeric(`Humidity intern (%relH)`),                                    # 9
                 Pressure_in                   = as.numeric(`Pressure intern (mbar)`),                                     #10
                 Temperature_ex                = as.numeric(`Temperature extern (degC)`),                                    #11
                 Humidity_ex                   = as.numeric(`Humidity extern (%relH)`),                                    #12
                 Pressure_ex                   = as.numeric(`Pressure extern (mbar)`),                                     #13
                 CO2.SCD30                     = as.numeric(`SCD30 CO2 (ppm)`),                                            #14
                 Temperature_in.SCD30          = as.numeric(`SCD30 Temperature (degC)`),                                     #15
                 Humidity_in.SCD30             = as.numeric(`SDC30 Humidity (%relH)`),                                     #16
                 CO2.GMP252                    = as.numeric(`Vaisala GMP252 CO2 (ppm)`),                                   #17
                 Water_content_soil.SMT100     = as.numeric(`SMT100 WaterContent (vol %)`),                                #18
                 Temperature_soil.SMT100       = as.numeric(`SMT100 Temperature (degC)`),                                    #19
                 Permittivity_coef_soil.SMT100 = as.numeric(str_remove(`SMT100 Permittivity dielectric coefficient`,";"))  #20

               )}else if(version=="v3.1"){

               #print("ok")
               out=((read_delim(path,delim = ";", escape_double = FALSE, trim_ws = TRUE,show_col_types = FALSE))[-1,])%>%
                 transmute(                                                                                                  #channel
                   Timestamp                     = as.POSIXct(Timestamp,format="%d.%m.%Y %H:%M:%S"),                         # 1
                   GPS_longitude                 = as.numeric(`GPS (longitude)`),                                            # 2
                   GPS_latitude                  = as.numeric(`GPS (latitude)`),                                             # 3
                   GPS_altitude                  = as.numeric(`GPS (altitude_m)`),                                           # 4
                   GPS_signal                    = as.logical(`GPS (fix)`),                                                  # 7
                   Temperature_in                = as.numeric(`Temperature intern (°C)`),                                    # 8
                   Humidity_in                   = as.numeric(`Humidity intern (%relH)`),                                    # 9
                   Pressure_in                   = as.numeric(`Pressure intern (mbar)`),                                     #10
                   Temperature_ex                = as.numeric(`Temperature extern (°C)`),                                    #11
                   Humidity_ex                   = as.numeric(`Humidity extern (%relH)`),                                    #12
                   Pressure_ex                   = as.numeric(`Pressure extern (mbar)`),                                     #13
                   CO2.SCD30                     = as.numeric(`SCD30 CO2 (ppm)`),                                            #14
                   Temperature_in.SCD30          = as.numeric(`SCD30 Temperature (°C)`),                                     #15
                   Humidity_in.SCD30             = as.numeric(`SDC30 Humidity (%relH)`),                                     #16
                   CO2.GMP252                    = as.numeric(`Vaisala GMP252 CO2 (ppm)`),                                   #17
                   Water_content_soil.SMT100     = as.numeric(`SMT100 WaterContent (vol %)`),                                #18
                   Temperature_soil.SMT100       = as.numeric(`SMT100 Temperature (°C)`),                                    #19
                   Permittivity_coef_soil.SMT100 = as.numeric(str_remove(`SMT100 Permittivity dielectric coefficient`,";"))  #20

                 )
               }
  else{
    errorCondition(message = "No match")
    return()
  }
  return(out)
}


#' @title laod_SEMACHv3_raw
#' @description A function to load one or multiple SEMACHv3 raw files and add them to a tibble. Useful for displaying CO2 slopes or sensor calibrations.
#' `r lifecycle::badge("experimental")`
#' @param path A file path to a SEMACHv3 .csv measurement file.
#' @param version SEMACH version for compatability with header read in. Default v3.2 (most recent systems)
#' @returns Formatted raw SEMACHv3 measurement files as a tibble. File name is used as ID.
#' @import tidyverse
#' @export
load_SEMACHv3_raw=function(path,
                           version="v3.2"
                           ){


  if(!str_detect(path,".csv")){
    files=list.files(path,full.names = T,pattern = ".csv")
  }
  else{
    files=path
  }
  output=c()
  for (file_i in files){
    measurement_ID=basename(file_i)%>%str_remove(".csv")
    measurement_data=(load_SEMACHv3_file(file_i,version))%>%mutate(measurement_time=as.numeric(seconds(Timestamp)-seconds(Timestamp[1])))

    output=rbind(output,
                 tibble(measurement_ID,
                        measurement_data)
    )

  }
  return(output)
}



#' @title eval_SEMACHv3
#' @description A function for the calculation of respiration fluxes from SEMACH files. (Similar to the way the python version `seanadamhdh/SEMACHv3` works).
#' `r lifecycle::badge("experimental")`
#' @param path A file path to a SEMACHv3 .csv measurement file.
#' @param height_offset Additional chamber volume will be calculated from extra height in m.
#' @param SCD30_scaling Scaling factor a for the SCD30 CO2 sensor for post-hoc calibration. (CO2.corr=a*CO2.SCD30+b). Default = 1, meaning no change.
#' @param SCD30_offset Scaling offset b for the SCD30 CO2 sensor for post-hoc calibration. (CO2.corr=a*CO2.SCD30+b) Default = 0, meaning no change.
#' @param cutoff_start The first n seconds of the measurement will be disregarded (equillibration period, 60s recommended).
#' @param cutoff_end Measurement will only be used to this point (in s). Long measurements might have non-linear behaviour.
#' @param A_chamber Chamber base area. For KG250 rings ~ .0434 m² (default).
#' @param V_chamber.default Default system volume. For SEMACHv3 0.01302 (KG250, 30 cm high, default), for SEMACHv2 0.01587
#' @returns Formatted raw SEMACHv3 measurement file
#' @import tidyverse
#' @export
eval_SEMACHv3=function(path,
                       height_offset=0,
                       SCD30_scaling=1,
                       SCD30_offset=0,
                       cutoff_start=61,
                       cutoff_end=3600,
                       A_chamber=.0434, #.0491 # note inner diameter is KG - 2s, with s = wall strength
                       V_chamber.default=.01302#.01587 # default = cylindrical volume with h=30cm
                       ){
  V_chamber=V_chamber.default+A_chamber*height_offset

  output=c()

  if(!str_detect(path,".csv")){
    files=list.files(path,full.names = T,pattern = ".csv",recursive = T)
  }
  else{
    files=path
  }

  for (file_i in files){
    measurement_ID=basename(file_i)%>%str_remove(".csv")

    measurement_data=load_SEMACHv3_file(file_i)


    full_duration=last(seconds(measurement_data$Timestamp))-first(seconds(measurement_data$Timestamp))
    true_cutoff_end=ifelse(full_duration>=cutoff_end,cutoff_end,full_duration)

    measurement_data=filter(measurement_data,
                            Timestamp>=(first(Timestamp)+seconds(cutoff_start))& #from
                              Timestamp<=(first(Timestamp)+seconds(cutoff_end))  # to
    )
    eval_duration=last(seconds(measurement_data$Timestamp))-first(seconds(measurement_data$Timestamp))

   # tibble(measurement_ID,
  #         full_duration,
   #        eval_duration,
    #       cutoff_start,
     #      cutoff_end=true_cutoff_end,
      #     height_offset,
       #    SCD30_scaling,
        #   SCD30_offset)%>%print
    if(nrow(measurement_data)>=5){



    #print(nrow(measurement_data))




    # for no, only implement corr method (T, P fluctuations expected)
    mutate(measurement_data,
           measurement_time=as.numeric(seconds(Timestamp)-seconds(Timestamp[1])), # seconds after start of recording
           CO2.SCD30_corr=(CO2.SCD30*SCD30_scaling+SCD30_offset)*Pressure_in*100/((273.15+Temperature_in.SCD30)*8.314), # using Pressure inside, no SCD30 data fro Pressure
           CO2.SCD30_corr_Tin=(CO2.SCD30*SCD30_scaling+SCD30_offset)*Pressure_in*100/((273.15+Temperature_in)*8.314), # using Pressure inside, no SCD30 data fro Pressure
           CO2.GMP252_corr=CO2.GMP252*Pressure_in*100/((273.15+Temperature_in)*8.314)
           #,           V_chamber=V_chamber.default+height_offset*A_chamber
    )%>%
      summarise(
        slope.SCD30=lm(CO2.SCD30_corr~measurement_time)[["coefficients"]][[2]],
        R2.SCD30=summary(lm(CO2.SCD30_corr~measurement_time))$r.squared,

        slope.SCD30_Tin=lm(CO2.SCD30_corr_Tin~measurement_time)[["coefficients"]][[2]],
        R2.SCD30_Tin=summary(lm(CO2.SCD30_corr_Tin~measurement_time))$r.squared,

        slope.GMP252=lm(CO2.GMP252_corr~measurement_time)[["coefficients"]][[2]],
        R2.GMP252=summary(lm(CO2.GMP252_corr~measurement_time))$r.squared,

        across(.cols=c(
                "GPS_longitude",
                "GPS_latitude",
                "GPS_altitude",
                "Temperature_in",
                "Humidity_in",
                "Pressure_in",
                "Temperature_ex",
                "Humidity_ex",
                "Pressure_ex",
                "Temperature_in.SCD30",
                "Humidity_in.SCD30",
                "Water_content_soil.SMT100",
                "Temperature_soil.SMT100",
                "Permittivity_coef_soil.SMT100"
        ),
        .fns=mean)
        )%>%
      mutate(
        F_CO2.SCD30=slope.SCD30*V_chamber/A_chamber,
        F_CO2.SCD30_Tin=slope.SCD30_Tin*V_chamber/A_chamber,
        F_CO2.GMP252=slope.GMP252*V_chamber/A_chamber,
        )->measurement_data

    output=rbind(output,
                 tibble(measurement_ID,
                        full_duration,
                        eval_duration,
                        cutoff_start,
                        cutoff_end=true_cutoff_end,
                        height_offset,
                        SCD30_scaling,
                        SCD30_offset,
                        measurement_data)
                 )
   # print(measurement_data)
  #  class(measurement_data)%>%print()
  }else{

    measurement_data=tibble(slope.SCD30=NA,R2.SCD30=NA,slope.SCD30_Tin=NA,
                              R2.SCD30_Tin=NA,slope.GMP252=NA,R2.GMP252=NA,
                              GPS_longitude=NA,GPS_latitude=NA,GPS_altitude=NA,
                              Temperature_in=NA,Humidity_in=NA,Pressure_in=NA,
                              Temperature_ex=NA,Humidity_ex=NA,Pressure_ex=NA,
                              Temperature_in.SCD30=NA,Humidity_in.SCD30=NA,
                              Water_content_soil.SMT100=NA,Temperature_soil.SMT100=NA,
                              Permittivity_coef_soil.SMT100=NA,F_CO2.SCD30=NA,
                              F_CO2.SCD30_Tin=NA,F_CO2.GMP252=NA)

    output=rbind(output,
                tibble(measurement_ID,
                       full_duration,
                       eval_duration,
                       cutoff_start,
                       cutoff_end=true_cutoff_end,
                       height_offset,
                       SCD30_scaling,
                       SCD30_offset,
                       measurement_data
                       )
    )



    }
  }
  return(output)
}







#'@title SEMACHv3 SCD30 Calibration
#'@description Calculation of slope + offset fro linear post-hoc calibration / correction of SEMACHv3 SCD30 data using GMP252 data
#' `r lifecycle::badge("experimental")`
#' @param dataset A single or multiple glued (i.e. bind_rows()) raw SMEACHv3 dataset loaded with `load_SEMACHv3_raw` (or equivalent). Must contain SCD30 and GMP252 data for at least a part of the dataset.
#' @param dir_path Path to folder containing subfolders with measurements (i.e. several days), or directly to a folder containing measurements
#' @param cutoff_start Of the individual measurements, the first seconds that should be omitted
#' @param cutoff_end Only use measurement data up to this time
#' @param plot Boolean. Should a calibration plot be returned? At least one of `plot` and `cal` should be True
#' @param cal Boolean. Should a result table of the calibrations be returned?
#' @param min_nrow Minimum number of datapoints for calculation
#' @import tidyverse
#' @export



SEMACHv3_calib=function(dataset=NULL,
                        dir_path=NULL,
                        cutoff_start=0,
                        cutoff_end=99999,
                        plot=F,
                        cal=T,
                        min_nrow=100){

  #print(plot) #debug
  #print(cal) #debug
  if (!is.null(dir_path)){
    if(!is.null(dataset)){
      warning("Both dataset and path provided. Using path.")
    }
    out = c()
    data_raw = c()
    if (dir.exists(dir_path)) {
      if (length(list.dirs(dir_path) > 1)) {
        # contains always ../ if exists
        print("subfolders")
        # subfolder loop
        for (i in list.dirs(dir_path)[-1]) {
          # contains always ../ if exists
          print(i)
          if (length(list.files(i)) > 0) {
            # files exist in dir
            data_raw = bind_rows(data_raw, load_SEMACHv3_raw(i))
            print("loaded")
          } else{
            print("empty")
          }
        }
      } else if (length(list.files(i)) > 0) {
        # files exist in dir
        print("single folder")
        data_raw = bind_rows(data_raw, load_SEMACHv3_raw(i))
        print("loaded")
      } else{
        print("empty")
      }
    } else{
      print("No data!")
    }
  } else if (!is.null(dataset)){
    print("Using dataset")
    data_raw=dataset
  } else{
    print("No data provided. Quitting")
    return()
  }

  #print(nrow(data_raw)) #debug
  data_raw%>%filter(!(measurement_time>cutoff_end|
                       measurement_time<cutoff_start|
                       CO2.SCD30==0|
                       CO2.GMP252==0) # bad vals
  )->data_raw

  #print(nrow(data_raw)) #debug

  if(nrow(data_raw)>min_nrow){
  res=lm(data=data_raw,formula=CO2.GMP252~CO2.SCD30)
  res_stat=summary(res)

  if(plot==T){
    (ggplot(res$model, aes(x = CO2.SCD30, y = CO2.GMP252)) +
       geom_abline(slope = 1) +
       geom_hex(binwidth = 10) +
       geom_abline(intercept = res$coefficients[[1]], slope = res$coefficients[[2]], col = "red") +
       geom_label(
         data = res$model %>% transmute(x = min(CO2.GMP252), y = max(CO2.SCD30)),
         hjust = 0, vjust = 1,
         aes(x = x, y = y),
         label = paste0(
           "GMP252 = ", format(res$coefficients[[1]], digits = 4), " + ",
           format(res$coefficients[[2]], digits = 4), " * SCD30", "\n",  # Use \n for line breaks
           "R2: ", format(res_stat$adj.r.squared, digits = 3), "\n",
           "StdErr: ", format(res_stat$sigma, digits = 3)
         )
       ) +
       scale_fill_viridis_c() +
       theme_minimal())->p
  }else{
    p=NULL
  }

  if(cal==T){
    cal_data=tibble(
      slope=res$coefficients[[2]],
      intercept=res$coefficients[[1]],
      R2=res_stat$adj.r.squared,
      StdErr=res_stat$sigma,
      n=nrow(data_raw)
    )
  }else{
    cal_data=NULL
  }
  if(plot&cal){
    return(list(p,cal_data))
  }else if(plot){
    return(p)
  }else if(cal){
    return(cal_data)
  }else{
    warning("No output selected")
    return(NULL)
  }

  }else{
    print("Too few observations.")
    return(NULL)
  }

}


#'@title SEMACHv3 raw data plotter
#'@description Display raw measurement data loaded by `load_SEMACHv3_raw`, or equivalent. Designed primarily for displaying CO2 measurement data, but can also be used for other data. Plots facet for each varaible.
#' `r lifecycle::badge("experimental")`
#' @param dataset Raw SEMACHv3 measurement file or glued set of measurement files (i.e., bind_rows()) loaded by `load_SEMACHv3_raw`, or equivalent.
#' @param variables Variables that are to be displayed. Can be single character or vector of character. If not available in dataset, warning will be displayed and entry is omitted. Default=c("CO2.SCD30")
#' @param cutoff_start Use data from when after measurement start (Default=0).
#' @param cutoff_end Use data to when after measurement start (Default=10800). If measurement duration is smaller, all available data is used.
#' @param rm_bad_SCD30 Should bad SCD30 measurements be omitted from the data (negative or 0 values). If SCD30 is not plotted, this will be disregarded. Default=T
#' @param rm_bad_GMP252n Should bad GMP252 measurements be omitted from the data (negative or 0 values). If GMP252 is not plotted, this will be disregarded. Default=T
#' @param leg_pos Argument handed to theme(legend.positon=leg_pos). Default="top". Set "none" to draw no legend. Details see `ggplot2::theme()`
#' @import tidyverse
#' @export

SEMACHv3_plot_raw=function(dataset,
                           variables="CO2.SCD30",
                           cutoff_start=0,
                           cutoff_end=10800,
                           rm_bad_SCD30=T,
                           rm_bad_GMP252=T,
                           leg_pos="top"
){

  if(!all(variables%in%names(dataset))){

    warning(paste0("Variables not available, skipping: ",
                   paste(variables[which(!variables%in%names(dataset))],collapse=", ")))
    variables=variables[which(variables%in%names(dataset))]

    }else if(cutoff_end<cutoff_start){
    warning("Abortings, cutoff_end < cutoff_start")
    return()
  } else{

    if(rm_bad_SCD30&any(c(variables)%in%"CO2.SCD30")){
      print(1) #debug
      dataset=dataset%>%filter(CO2.SCD30>0)
    }
    if(rm_bad_GMP252&any(c(variables)%in%"CO2.GMP252")){
      print(2) #debug
      dataset=dataset%>%filter(CO2.GMP252>0)
    }

    if((nrow(dataset)<10)){
      nrow(dataset)
      warning("Abortings, too few datapoints")
      return()
    } else{

      dataset%>%
        filter(!(measurement_time>cutoff_end|measurement_time<cutoff_start))%>%
        pivot_longer(cols=all_of(c(variables)))%>%
        ggplot(aes(x=measurement_time,col=measurement_ID,y=value))+
        geom_line()+theme_minimal()+
        theme(legend.position = leg_pos)+
        facet_wrap(~name,scales="free_y")
    }
  }
}




#' @title SEMACHv3 plot results
#' @description Function for displaying eval_SEMACHv3 result files
#' `r lifecycle::badge("experimental")`
#' @param dataset A results tibble returned by `SEMACHv3_batch_evaluation`
#' @param pathlist Character vector containing path(s) to csv file(s) containing saved SEMACHv3 results (python SEMACHv3.exe output).
#' Folder structure described in `SEMACHv3_batch_evaluation`needs to be followed to work properly.
#' @param type For compatability of python SEMACHv3.exe (=P), and R results (=R, default)
#' @import tidyverse
#' @import ggpubr
#' @export
SEMACHv3_plotRes=function(pathlist=NULL,dataset=NULL,type="R"){

  if(!is.null(dataset)){

    if(!is.null(pathlist)){
      warning("Both pathlist and dataset provided. Using only dataset")
    }

    semachv3=dataset

    } else if(!is.null(pathlist)){

    semachv3=c()
    for (i in pathlist){
      semachv3=bind_rows(
        semachv3,
        read_csv(i)%>%mutate(Timestamp=as.POSIXct(Timestamp,format="%d-%m-%Y %H:%M:%S"),
                             System=paste0("Ch-",str_split(i,"Ch-")[[1]][2]%>%substr(1,1)))
      )
      }
    }else{
      warning("Aborting: No data provided")
      return()
    }

  if(type=="R"){
    semachv3=rename(semachv3,
                    temperature_in = Temperature_in,
                    temperature_ex = Temperature_ex,
                    temperature_in_scd30 = Temperature_in.SCD30,
                    humidity_in = Humidity_in,
                    humidity_ex = Humidity_ex,
                    humidity_in_scd30 = Humidity_in.SCD30,
                    pressure_in = Pressure_in,
                    pressure_ex = Pressure_ex,
                    f_co2_gmp252 = F_CO2.GMP252,
                    f_co2_scd30_corr = F_CO2.SCD30,
                    f_co2_scd30_corr_tscd = F_CO2.SCD30_Tin,
                    System=ch_sys
                    )%>%
      mutate(Timestamp=as.POSIXct(measurement_ID,format="%d%m%Y-%H%M%S"))
  }




  ggarrange(
    plotlist=list(
      ggplot(semachv3%>%
               pivot_longer(cols=c(temperature_in,temperature_ex,temperature_in_scd30)),
             aes(x=Timestamp,y=value,col=name,shape=System,linetype = System))+
        geom_point()+geom_line()+theme_pubclean()+ylab("Temperature °C")
      ,

      ggplot(semachv3%>%
               pivot_longer(cols=c(humidity_in,humidity_ex,humidity_in_scd30)),
             aes(x=Timestamp,y=value,col=name,shape=System,linetype = System))+
        geom_point()+geom_line()+theme_pubclean()+ylab("relLF %")

      ,
      ggplot(semachv3%>%
               pivot_longer(cols=c(pressure_in,pressure_ex)),
             aes(x=Timestamp,y=value,col=name,shape=System,linetype = System))+
        geom_point()+geom_line()+theme_pubclean()+ylab("Pressure hPa")

      ,
      ggplot(semachv3%>%
               pivot_longer(cols=c(f_co2_gmp252,f_co2_scd30_corr,f_co2_scd30_corr_tscd)),
             aes(x=Timestamp,y=value,col=name,shape=System,linetype = System))+
        geom_point()+geom_line()+theme_pubclean()+ylab("FCO2 umol m-2 s-1")
    )
  )

}




#' @title SEMACHv3 batch evaluation
#' @description Wrapper for streamlined evaluation of larger sets of respiration measurements.
#' Requires proper folder structure for Chambers / Measurement data.
#' `r lifecycle::badge("experimental")`
#' @param dirpath Path to parent folder.
#' ./dirpath should contain folders for each chamber named as stated in ch_list. See details.
#' @param metadata_path Path to a .csv file containing slope and offset for each chamber system.
#'  Must contain at least 4 columns:
#'  `System` - the chamber system id used in the folder structure,
#'  `SCD30_slope` - the slope correction for SCD30 data,
#'  `SCD30_offset` - the offset correction for SCD30 data,
#'  `height_offset` -  height difference of system volume. Note that this can here only be changed for each subfolder, not each ring / measurement site.
#'  SCD30 slope and offset can be calculated by `SEMACHv3_SCD30calib`.
#'  If data should be processed without corrections, entries of `SCD30_slope` have to be set to 1, and both `SCD30_offset` and `height_offset` to 0.
#' @param ch_list Character vector of individual chamber systems. Must be same as folder names in dirpath and entries column System in cal_lookup_path.
#' @param cutoff_start The first n seconds of the measurement will be disregarded (equilibration period, 60s recommended).
#' @param cutoff_end Measurement will only be used to this point (in s). Long measurements might have non-linear behaviour.
#' @param from Only evaluate chamber data recored after this date.
#' @param to Only evaluate chamber data recored before this date.
#' @details Currently, no way of attributing an site or measurement id to chamber measurements is implemented. The only way to identify measurements is the timestamp that is used as filename. Therefore, a clear folder structure is paramount to differentiate between chamber systems and  allow the assignment of measurement to individual sites.
#' Data therefore recommended to be stored in a folder structure as follows:
#'
#'
#'      ./dirpath
#'
#'        /Ch-1
#'
#'            /date1
#'
#'                measurement1.csv
#'
#'                measurement2.csv
#'
#'                ...
#'
#'        /Ch-2
#'
#'        ...
#'
#'        /metadata.csv
#'
#'
#' @import tidyverse
#' @export


SEMACHv3_batch_evaluation = function(dirpath="//zfs1.hrz.tu-freiberg.de/fak3ibf/Hydropedo/field_data/data/SEMACHv3/SEMACHv3.1.0/",
                                     metadata_path=paste0(dirpath,"metadata.csv"),
                                     ch_list=paste0("Ch-",c(1:9)),
                                     cutoff_start = 181,
                                     cutoff_end = 99999,
                                     from="2024-01-01",
                                     to="3000-01-01"
) {



  metadata=read_csv(metadata_path)
  #print(metadata) #debug
  out = c()

  for (ch_sys in ch_list) {
    print(ch_sys)
    dirlist = list.dirs(
      paste0(
        dirpath,
        ch_sys,
        "/"
      )
    )
    dates = basename(dirlist) %>% as.POSIXct(format = c("%Y-%m-%d"))
    sel_dir = dirlist[which(dates >= as.POSIXct(from) &
                              dates <= as.POSIXct(to))]
    for (folder in sel_dir) {
      print(basename(folder))


      result=eval_SEMACHv3(
        path = folder,
        SCD30_scaling = filter(metadata, System == ch_sys) %>% pull(SCD30_slope),
        SCD30_offset = filter(metadata, System == ch_sys) %>% pull(SCD30_offset),
        cutoff_start = cutoff_start,
        cutoff_end = cutoff_end,
        height_offset = filter(metadata, System == ch_sys) %>% pull(height_offset)
      )
      #print(result) #debug
      out=bind_rows(out,
                    mutate(result,
                           ch_sys=ch_sys
                    )
      )
    }
  }
  return(out)
}






