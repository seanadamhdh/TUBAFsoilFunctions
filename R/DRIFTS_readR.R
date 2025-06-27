
#' @title Load and pre-process OPUS raw spectra files (file.O)
#' @description This function loads a set of raw opus spectra files into R, processes them into a nested tibble format (propectr-friendly),
#' and applies several pre-processing techniques.
#' `r lifecycle::badge("stable")`
#' @param folder a folder containing the raw spectra
#' @param save_location where should the produced files be saved
#' @param save logical: should the final output be saved as RDS in 'save_location'?
#' @param save_raw logical: should the loaded raw spectra be saved as RDS in 'save_location'?
#' This can be used to reload the raw spectra when calling this function again, which can save quite some time
#' when loading a large amount of spectra
#' @param return logical: should the final output be returned by the function? Set T, when using in pipeline, or assigning
#' to variable. Otherwise set F, or output will be printed in console and cause lag.
#' @param id_string_location Which characters in the sample column strings are the unique sample identifeirs?
#' Provide a vetor of start and end position. E.g., default for BDF spec: Full file names are:
#' "BDF-XXXX-y....., with XXXX being the sample name. Therefore, default is set to c(5,8)
#' @param spc_cols Locates the raw spc cols in the aggregated raw spectra dataframe. Default is for
#' scans from 7500-400 cm-1 at 1 cm-1 (actually at about 0.6 cm-1), yielding 13860 spc columns.
#' You can input either the total No. of spc_cols (e.g. 13860), the start and end index of the spc cols (c(5,13864)),
#' or the full vector of spc col indices (c(5:13864)).
#' @param reload Is a 'raw_spc' RDS file stored in 'save_location' from a previous run you want to load instead of reading
#' raw opus files?
#' @param make_upper Sample names are converted to fully uppercase (removes case typos). Parametrisation for compatability (option to turn it off)
#' @param date_splits Vector of dates in string format uses to split all raw opus files into groupes for read-in. This is used because recalibration of the FT wavenumbers
#' introduces a small shift which hinders direct merge of spectra by wavenumbers. Subsets are remerged after rs to 1 cm-1.
#' Date_split string format should conform to default Posixct format. Either "YYYY-MM-DD" or "YYYY-mm-dd HH:MM:SS".
#' @import simplerspec
#' @import tidyverse
#' @import prospectr
#' @import resemble
#' @returns If save_raw=T and reload=F, the initially read-in raw opus files are saved after gathering  (details see simplerspec documentation) them as a rds object in save_location as raw_spc.
#' @returns If save=T, the final object containing the differently pre-processed spectra sets is saved in save_location as spc_data.
#' @returns If return=T, the final object containing the differently pre-processed spectra is
#' @export
OPUSraw_to_Preprocessed<-function(
    folder="../../1_raw_scans/scans_main", #!!!<-path to folder that contains the spectra
    save_location="./temp",#!!!<-path to whereever you want to save tables / in-between saves etc.
    save=T,
    save_raw=T,
    return=T,
    id_string_location=c(5,8),
    from=400,
    to=7500,
    # for 7500:400 @ 1cm-1 (in raw spc wierdly around 0.6 actually)
    reload=F,
    make_upper=T,
    date_splits=c("2024-10-07")
){
  if(!reload){
    # get filenames with full directory
    fnames<-paste(folder,
                  list.files(folder),
                  sep="/")

    # read raw opus data
    raw_scans<-read_opus_univ(fnames)

    # check for duplicates, should be FALSE
    if(any(raw_scans%>%names%>%duplicated())){
      cat("\nWARNING: Duplicate files found. You should check your data.\nContinuing anyway...")
    }

    # combine in readable dataset
    raw_spc<-gather_spc(raw_scans)

    # save raw input to avoid lengthy load-in
    if(save_raw){
      saveRDS(raw_spc,paste(save_location,"raw_spc",sep="/"))
    }
  }else{
    raw_spc<-readRDS(paste(save_location,"raw_spc",sep="/"))
  }

  print("spc loaded")


  dt_start=as.POSIXct("1800-01-01 00:00:00")
  spc_data=c() # init
  dt_dataframe=transmute(raw_spc,
                         file_id=file_id,
                         dt=str_remove(unique_id,file_id)%>%str_remove("_")%>%as.POSIXct()  # unique id always has _dt appended to file name (which als ocontains date, but due to sample naming)
  )
  #print(dt_dataframe) #debug
  count_i=0
  for (dt_end in c(as.POSIXct(date_splits), as.POSIXct("3000-01-01 00:00:00"))){
    count_i=count_i+1
    print(paste("formatting split",count_i))
    #print(which(dt_dataframe$dt>dt_start&dt_dataframe$dt<=dt_end)) #debug
    raw_spc_i=raw_spc[which(dt_dataframe$dt>dt_start&dt_dataframe$dt<=dt_end),]


    if(as.logical(nrow(raw_spc_i))){ #check if any spc to process in subset

      no_of_spc_cols_raw<-length(raw_spc_i$wavenumbers[[1]])
      spc_cols<-c(5:no_of_spc_cols_raw)


      # shortening the identifer col to sample name (select position with the id_string_location argument)
      if(length(id_string_location)==2){
        #from-to argument
        raw_spc_i$sample_id<-substr(raw_spc_i$sample_id,id_string_location[1],id_string_location[2])
      }else{
        # not great, but at least some catch
        cat("Warning: Less or more than 2 string_id_location positions supplied. Using sample_id string from first string_id_location onwards.")
        raw_spc_i$sample_id<-substr(raw_spc_i$sample_id,id_string_location[1],10000L) # saves longer entries also, takes first
      }

      # ID fully uppercase
      if(make_upper==T){
        raw_spc_i$sample_id=toupper(raw_spc_i$sample_id)
      }


      # formatting 1: from simplerspec to full unnested
      # introduces NA for different raw spc wn -> fix by splitting loop

      #print(glimpse(raw_spc_i))
      unnest(raw_spc_i,spc)->unnested_spc

      #print(glimpse(unnested_spc))
      # formatting 2: prospectr friendly
      # averaging repetition spectra
      spc<-tibble(unnested_spc)%>%select(-unique_id,-file_id,-metadata,-wavenumbers)%>% #reatins wn, does no harm
        group_by(sample_id)%>%
        summarise_all(mean) #simple averageing of replicate scans
      #print(names(spc$spc))
      #print(3) #debug

      spc0<-spc[(spc_cols-3)] # !!! pulling spc columns
      spc_data_i<-tibble(spc[1],spc=spc0) #and reintroduicing them as a nested matix (prospectr-friendly)


      # metadata, ! using always metadata of first repetiton
      spc_data_i$metadata<-unnested_spc%>%group_by(sample_id)%>%summarise(metadata=first(metadata))



      # resampling 7500-400 @ 1 cm-1 resolution
      spc_data_i$spc_rs<-resample(spc_data_i$spc,
                                  wav=as.numeric(names(spc_data_i$spc)),
                                  new.wav=seq(to, #start !!!
                                              from,  #stop  !!!
                                              -1    #step  !!!
                                  ))

      #print(4) #debug
      # generalising colnames to allow merging across differentl calibrated raw spc. Original raw wn shoul still be obtainable fom metadata
      colnames(spc_data_i$spc)=c(1:length(raw_spc_i$wavenumbers[[1]]))

      spc_data=bind_rows(spc_data,spc_data_i)
      # reassigning rs colnames - getting lost during rowbinding
      colnames(spc_data$spc_rs) = seq(to, #start !!!
                                      from,  #stop  !!!
                                      -1    #step  !!!
      )
    }
    dt_start=dt_end+days(1)

  }

  ################################# ##
    print("spc formatted")

  # Savitzky-Golay smoothing
  spc_data$spc_sg<-savitzkyGolay(spc_data$spc_rs,
                                 m=0,  # m-th derivative (0 = no derivative)
                                 p=3,  # polynomial order
                                 w=21  # window size (should be odd)
  )


  spc_data$spc_sg_snv<-standardNormalVariate(spc_data$spc_sg)



  if(nrow(spc_data$spc_sg)>1){
    spc_data$spc_sg_bl<-baseline(spc_data$spc_sg,
                                 wav = as.numeric(colnames(spc_data$spc_sg))
    )
  }else{ # workaround, fixed named vector JSON issue =)... baseline returns named numeric for single row inputs, which messed up a lot
    spc_data$spc_sg_bl<-baseline(spc_data$spc_sg,
                                 wav = as.numeric(colnames(spc_data$spc_sg)))%>%
      as.list()%>%as_tibble%>%as.matrix()
  }

  spc_data$spc_sg_bl<-spc_data$spc_sg_bl[,-c(1,ncol(spc_data$spc_sg)),drop=F] #rm first and last

  # resampling baseline 7500-400 @ 4 cm-1 resolution
  spc_data$spc_sg_bl_rs4<-resample(spc_data$spc_sg_bl,
                                   wav=as.numeric(colnames(spc_data$spc_sg_bl)),
                                   new.wav=seq(to-14, #start !!!
                                               from+14,  #stop  !!!
                                               -4    #step  !!!
                                   ))



  # resampling SNV 7500-400 @ 4 cm-1 resolution
  spc_data$spc_sg_snv_rs4<-resample(spc_data$spc_sg_snv,
                                    wav=as.numeric(colnames(spc_data$spc_sg_snv)),
                                    new.wav=seq(colnames(spc_data$spc_sg_snv)%>%as.numeric%>%max, #start !!!
                                                colnames(spc_data$spc_sg_snv)%>%as.numeric%>%min,  #stop  !!!
                                                -4    #step  !!!
                                    ))
  #### raw resampled ####
  spc_data$spc_rs4=resample(spc_data$spc_rs,
                            wav = as.numeric(colnames(spc_data$spc_rs)),
                            new.wav = seq(max(as.numeric(colnames(spc_data$spc_rs))),
                                          min(as.numeric(colnames(spc_data$spc_rs))),
                                          -4))

  #### sgresampled ####
  spc_data$spc_sg_rs4=resample(spc_data$spc_sg,
                               wav = as.numeric(colnames(spc_data$spc_sg)),
                               new.wav = seq(max(as.numeric(colnames(spc_data$spc_sg))),
                                             min(as.numeric(colnames(spc_data$spc_sg))),
                                             -4))

  #### add 1st derivative set ####
  spc_data$spc_sg1d=savitzkyGolay(spc_data$spc_rs,
                                  m=1,
                                  p=3,
                                  w=41 #larger window size to counter suceptibility against noise
  )

  spc_data$spc_sg1d_rs4=resample(spc_data$spc_sg1d,
                                 wav = as.numeric(colnames(spc_data$spc_sg1d)),
                                 new.wav = seq(max(as.numeric(colnames(spc_data$spc_sg1d))),
                                               min(as.numeric(colnames(spc_data$spc_sg1d))),
                                               -4))


  #### add 2nd derivative set ####
  spc_data$spc_sg2d=savitzkyGolay(spc_data$spc_rs,
                                  m=2,
                                  p=3,
                                  w=41 #larger window size to counter suceptibility against noise
  )

  spc_data$spc_sg2d_rs4=resample(spc_data$spc_sg2d,
                                 wav = as.numeric(colnames(spc_data$spc_sg1d)),
                                 new.wav = seq(max(as.numeric(colnames(spc_data$spc_sg1d))),
                                               min(as.numeric(colnames(spc_data$spc_sg1d))),
                                               -4))

  print("spc preprocessed")

  if(save){
    saveRDS(spc_data,paste(save_location,"spc_data",sep="/"))
  }
  if(return){
    return(spc_data)
  }
}








#' @title Spectra plotter
#' @description Simple plotting interface for spc datasets created with OPUSraw_to_preprocessed(). Note: Large amount of spc might be laggy.
#' Subset data beforehand or set sample size to smaller number for random sampling.
#' `r lifecycle::badge("experimental")`
#' @param dataset a spc dataset, as created by OPUSraw_to_Preprocessed
#' @param spc_id The id of the nested spc matrix. Choose from c("spc","spc_rs","spc_sg","spc_sg_bl",
#' "spc_sg_bl_rs4","spc_sg_snv","spc_sg_snv_rs4")
#' @param sample_size How many spectra should be plotted? Default is all. If lower than nrow(dataset), spc are
#' sampled randomly. To select specific spc, filter dataset beforehand.
#' @param interactive logical: If TRUE, a ggplotly interactive plot will be created.
#' @param reverse_x logical: If TRUE, x-axis will be plotted in reverse (decending wavenumber)
#' @param col_var str to selcet variable column for spc coloring
#' @param leg_pos legend position arg. handed to ggplot legend.position
#' @import plotly
#' @import tidyverse
#' @export
spectra_plotter<-function(
    dataset, #<-a tibble / dataframe with the nested spectra matrices, as we created above
    spc_id="spc_rs",#<-the spectra set / matrix you want to plot (as string)
    sample_size=nrow(dataset),#<-sample size, defaults to all spectra, but this might be slow for large sets
    interactive=F, # plot as interactive ggplotly in the viewer tab? if not simple ggplot (default)
    reverse_x=T, # reverse x-Axis scale? default is T,
    col_var="ID",
    leg_pos="none"
){
  # the magic... to lazy to annotate everything
  dataset<-dataset[sample(nrow(dataset),sample_size),]
  bind_cols(ID=dataset[["sample_id"]],col_var=dataset[[col_var]],dataset[[spc_id]])%>%
    pivot_longer(
      cols = colnames(.)[-c(1,2)], #not ID, col_var
      names_to = "wavenumber",
      names_transform = list("wavenumber"=as.numeric),
      values_to = "absorbance"
    )%>%
    ggplot(aes(x=wavenumber,y=absorbance,col=col_var,group=ID))+ #group_id for individual spc lines
    geom_line(alpha=.1)+
    theme_minimal()+
    scale_color_discrete(col_var)+
    theme(legend.position = leg_pos)->plt
  if(reverse_x==T){
    plt<-plt+scale_x_reverse()
  }
  if(interactive==T){
    ggplotly(plt)
  }else{
    plot(plt)
  }
}

# run examples (you need to load your spectra first)
if(F){
  # simple plot of 3 spectra
  spectra_plotter(dataset=spc_data,
                  spc_id = "spc_rs",
                  sample_size = 3,
                  interactive = F,
                  reverse_x = T)

  # interactive plot
  spectra_plotter(dataset=spc_data,
                  spc_id = "spc_rs",
                  sample_size = 3,
                  interactive = T,
                  reverse_x = T)

  # plot all
  spectra_plotter(dataset=spc_data,
                  spc_id = "spc_rs",
                  interactive = T,
                  reverse_x = T)
}
