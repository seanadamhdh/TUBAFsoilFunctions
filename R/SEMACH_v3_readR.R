


source(paste0(root_dir,"/GitHub/R_main/R_main/packages.R"))
source(paste0(root_dir,"/GitHub/R_main/R_main/load_PriEco8800.R"))
if(!require(useful)){
  install.packages("useful")
  require(useful)
}


#' @title load_SEMACHv3_file
#' @description
#' A function for loading and formatting SEMACHv3 csv measurement files.
#'
#' @param path A file path to a SEMACHv3 .csv measurement file.
#' @returns Formatted raw SEMACHv3 measurement file
#' @import tidyverse
#'
#'
load_SEMACHv3_file=function(path){
  return(
    ((read_delim(path,delim = ";", escape_double = FALSE, trim_ws = TRUE,show_col_types = FALSE))[-1,])%>%
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
  )
}

#' @title laod_SEMACHv3_raw
#' @description
#' A function to load one or multiple SEMACHv3 raw files and add them to a tibble.
#' @param path A file path to a SEMACHv3 .csv measurement file.
#'
#' @returns Formatted raw SEMACHv3 measurement files as a tibble. File name is used as ID.
#' @import tidyverse
#'

load_SEMACHv3_raw=function(path){


  if(!str_detect(path,".csv")){
    files=list.files(path,full.names = T,pattern = ".csv")
  }
  else{
    files=path
  }
  output=c()
  for (file_i in files){
    measurement_ID=basename(file_i)%>%str_remove(".csv")
    measurement_data=(load_SEMACHv3_file(file_i))%>%mutate(measurement_time=as.numeric(seconds(Timestamp)-seconds(Timestamp[1])))

    output=rbind(output,
                 tibble(measurement_ID,
                        measurement_data)
    )

  }
  return(output)
}



#' @title eval_SEMACHv3
#' @description A function for the calculation of respiration fluxes from SEMACH files.
#' @param path A file path to a SEMACHv3 .csv measurement file.
#' @param height_offset Additional chamber volume will be calculated from extra height in m.
#' @param SCD30_scaling Scaling factor a for the SCD30 CO2 sensor for post-hoc calibration. (CO2.corr=a*CO2.SCD30+b)
#' @param SCD30_offset Scaling offset b for the SCD30 CO2 sensor for post-hoc calibration. (CO2.corr=a*CO2.SCD30+b)
#' @param cutoff_start The first n seconds of the measurement will be disregarded (equillibration period, 60s recommended).
#' @param cutoff_end Measurement will only be used to this point (in s). Long measurements might have non-linear behaviour.
#'
#' @returns Formatted raw SEMACHv3 measurement file
#'
#'@import tidyverse

eval_SEMACHv3=function(path,
                       height_offset=0,
                       SCD30_scaling=1.129,
                       SCD30_offset=23.953,
                       cutoff_start=61,
                       cutoff_end=3600){

  A_chamber=.0491 #fixed
  V_chamber.default=.01587 #fixed

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




# testing ####
if(F){
eval_SEMACHv3(path="~/GitHub/R_main/data/SEMACHv3/",SCD30_correction_factor = 1)->test

rbind(
  tibble(loc="L",load_SEMACHv3_raw("~/GitHub/R_main/data/SEMACHv3/2024-08-16 Wiese Labor")),
  tibble(loc="W",load_SEMACHv3_raw("~/GitHub/R_main/data/SEMACHv3/2024-08-16 Wiese Zentrale Werkstatt/")),
  tibble(loc="E",load_SEMACHv3_raw("~/GitHub/R_main/data/SEMACHv3/2024-08 Eimer Labor/"))
  )->SEMACHraw

ggplotly(
SEMACHraw%>%group_by(measurement_ID)%>%mutate(CO2.SCD30=(CO2.SCD30-first(CO2.SCD30))*1.15, # scaling factor
                                              CO2.GMP252=CO2.GMP252-first(CO2.GMP252))%>%pivot_longer(cols=c(CO2.SCD30,CO2.GMP252))%>%
  ggplot(aes(x=measurement_time,col=loc,group=measurement_ID,
             shape=Name,y=value))+geom_point()+geom_line()
)
}






