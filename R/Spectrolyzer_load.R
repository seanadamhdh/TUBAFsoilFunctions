

#' @title Spectrolyzer fingerprint loader
#' @description NOTE: Removes flagged spectra. Generalised fingerprint load-in for spectrolyzer data.
#' `r lifecycle::badge("experimental")`
#' @param directory Path to unzipped fingerprint-folder.
#' @param wavelengths Vector of wavelengths. Column names of measured wavelengths. default is 200-750 nm in 2.5 nm steps
#' @param sel_wavelengths Vector of wavelengths. Range of wavelengths that shall be returned (<= range of `wavelengths`)
#' @param id Boolean. Should serial No be added as column to the tibble (for ID-ing the spectrolyzer later). Default=T. For addition of id-column, the metadata-file must be present in `directory`
#' @returns A nested tibble with the columns Date_Time (POSIXct), spc with spc$`sel_wavelengths` (absorbances at wavelength) and, if `id`==TRUE, the Serial_No, containing the spectrolyzer id.
#' @import tidyverse
#' @export
#'
Spectro_load_fingerprint<-function(directory,
                                   wavelengths=seq(200,750,2.5),
                                   sel_wavelengths=seq(200,722.5,2.5),
                                   id=T){

  # load file
  temp=suppressMessages(#silent read-in
    read_csv(list.files(directory,pattern = ".csv",full.names = T), #actual filenames
    col_types = cols(Timestamp = col_datetime(format = "%Y-%m-%dT%H:%M:%S+0000")))
  )

  # rename cols
  names(temp)<-c("Date_Time",format(wavelengths,nsmall=1),"Flags")


  # POSIXct format
  temp$Date_Time<-as.POSIXct(temp$Date_Time)

  # remove flagged cols (e.g. "NO MEDIUM" Error, but also other possible error-rows)
  subset(temp,is.na(Flags))%>%

    # rm Flags (did it's job)
    select(-c("Flags"))%>%

    # rm nan cols (no data recorded for these wavelengths)
    # using first() because i guess it's faster
    # if issues come up replace with all()
    select_if(~is.nan(first(.))!=T)->temp
    # select desired range and format as nested tibble
    out=tibble(Date_Time=temp$Date_Time,spc=select(temp,format(sel_wavelengths,nsmall=1)))
  if(id==T){
    Serial_No<-read.delim(list.files(directory,pattern = ".json",full.names = T), #actual filename
    header=FALSE)[1,1]%>%substr(str_locate(.,"sensorSerial:")[2]+1, # start of serialNo.
                                str_locate(.,"sensorSerial:")[2]+8) # end of serialNo, assuming 8 digits
    out$Serial_No<-rep(Serial_No,length(out[[1]]))
  }

  return(out)
}

#' @title Rename flag columns
#' @description Helper function for renaming Flag columns in spectrolyzer parameter files. Intended for internal use.
#' Colnames should be structured as ..., var1, Flags...n, var2, Flags...n, ... where n is the column index automatically assigned by read_csv
#' `r lifecycle::badge("experimental")`
#' @param paramter_df A raw parameter file loaded with read_csv(...)
#' @returns Tibble with colnames changed to ..., var1, Flags_var1, var2, Flags_var2, ...
#' @import tidyverse
#' @export
rename_flags <- function(parameter_df) {

  cols <- colnames(parameter_df)

  for (i in seq_along(cols)) {
    if (grepl("^Flags\\.\\.\\.\\d+$", cols[i]) && i > 1) {
      new_name <- paste0("FLAG_", cols[i - 1])
      cols[i] <- new_name
    }
  }
  colnames(parameter_df) <- cols

  return(parameter_df)
}


#' @title Spectrolyzer parameter loader
#' @description NOTE: Removes flagged spectra. Generalised parameter load-in for spectrolyzer data.
#' `r lifecycle::badge("experimental")`
#' @param directory Path to unzipped parameter-folder.
#' @param id Boolean. Should serial No be added as column to the tibble (for ID-ing the spectrolyzer later). Default=T. For addition of id-column, the metadata-file must be present in `directory`
#' @returns A tibble with the columns Date_Time (POSIXct), `parameters` (additionally measured variables) and, if `id`==TRUE, the Serial_No, containing the spectrolyzer id.
#' @import tidyverse
#' @export
Spectro_load_parameter<-function(directory,
                                 parameters=c("Temp"),
                                 id=T){
  # load file
  temp<-suppressMessages( #silent
    read_csv(list.files(directory,pattern = ".csv",full.names = T), #actual filename
      col_types = cols(Timestamp = col_datetime(format = "%Y-%m-%dT%H:%M:%S+0000"),
                   ))
  )


  temp=rename_flags(temp)

  # LEGACY: MANUAL renaming
  # # rename cols
  # # fix for multiple Flag cols
  # namelist=c("Date_Time")
  # for (i in parameters){
  #   namelist=c(namelist,i,paste0("Flags_",i))
  # }
  #
  # # additional dummy colnames
  # namelist=c(namelist,c((length(namelist)+1):length(temp)))
  # print(namelist)
  # names(temp)<-namelist

  if(id==T){
    Serial_No<-read.delim(list.files(directory,pattern = ".json",full.names = T), #actual filename
                          header=FALSE)[1,1]%>%substr(str_locate(.,"sensorSerial:")[2]+1, # start of serialNo.
                                                      str_locate(.,"sensorSerial:")[2]+8) # end of serialNo, assuming 8 digits
    temp$Serial_No<-rep(Serial_No,length(temp[[1]]))
  }


  # POSIXct format
  temp=(temp%>%rename(Date_Time=Timestamp))%>%mutate(across(Date_Time,as.POSIXct))

  return(temp)
}



#' @title Merge fingerprint and parameter data
#' @details Merging of fingerprint and parameter files loaded with Spectro_load_fingerprint and Spectro_load_parameter.
#' Intended primarily for internal use.
#' @param fingerprint fingerprint tibble, created by Spectro_load_fingerprints
#' @param parameters fingerprint tibble, created by Spectro_load_parameters
#' @note Warning messages: "Unknown or initialised colum: `Serial_No` ...
#' known warning, function switches automatically to alternate case.
#' @returns Tibble with merged fingerprint and parameter data.
#' @import tidyverse
#' @export
Spectro_merge_col<-function(fingerprint,parameter){
  # case1 both have serial no. fingerprint Serial_No is used for merging alongside Date_Time
  if(is.null(fingerprint$Serial_No)==F&
     is.null(parameter$Serial_No)==F){

    #Serial_No<-fingerprint$Serial_No

    temp<-left_join(fingerprint,parameter,by=c("Date_Time","Serial_No"))
    #temp$Serial_No<-Serial_No

    # case 2 either only one set contains Serial_No. Joining by date only.
    # Serial_No is joined if available for one of the sets.
  }else{
    temp<-left_join(fingerprint,parameter,by=c("Date_Time"))
  }
  return(temp)
}



#' @title Unzip helper
#' @description Unzips .zip folders and saves locally. Internal, i.e., for processing of Spectrolyzer data.
#' @param zip_directory zip-folder that is to be extracted, i.e.: `./somepath/folder/data.zip`
#' @param new Target folder. Default is NULL. If NULL, unzipped folder is written in the same place as the zip folder,
#'  with the same name as the zip folder,i.e.: `./somepath/folder/data`
#' If not NULL, unzipped folder `data` is written in `new`. IF `new` does not exist, it is created.
#' @import utils
#' @export
unzip_local<-function(zip_directory,new_dir=NULL){
  if(is.null(new_dir)==F){
    # when new dir is given, this creates the path
    new_dir<-paste0(dirname(zip_directory),"/",new_dir,"/")
    # checks if new_dir already exists, gets rid of annoying warnings
    if(dir.exists(new_dir)==F){
      dir.create(new_dir)
    }
    unzip(zip_directory,exdir = paste0(new_dir,str_remove(basename(zip_directory),".zip")))
  }else{
    # same folder as for zip file
    unzip(zip_directory,exdir = str_remove(zip_directory,".zip"))
  }
}



#' @title Spectrolyzer data loader
#' @description Wrapper function for streamlined load-in of spectrolyzer data.
#' Enables load-in of multipe scpectrolyzer-zip-files or already un-zipped files to coherent tibble
#' `r lifecycle::badge("experimental")`
#' @param parent_dir path of folder containing the zip- or un-zipped files
#' @param wavelengths col-names of measured wavelengths. default is 200-750 in 2.5 m, steps. Details see \link{Spectro_load_fingerprint}
#' @param sel_wavelengths Range of wavelengths that shall be returned. Details see \link{Spectro_load_fingerprint}
#' @param zip Boolean. Are zip-files (T) in parent_dir, or are folders already unzipped(F)? Default is zip=T.
#' @param exclude Optional character vector of subfolders to be disregarded.
#' @param read_param If TRUE, parameter fiels are read in, else skipped (...fix issues before setting TRUE)
#' If so, list them in a vector and they will be disregarded.
#' @note if zip files are loaded, the unzipped folders are put in a new parent folder called "data" inside
#' of the original directory
#' @details Note that there might be a many-to-many relations warning when merging the individual spectrolyzer subsets. This is expected and duplicates are removed before returning the dataset.
#' @returns A tibble that contains all fingerprint and parameter data, joined by Serial_No and Date_Time. Duplicated mearsurements (i.e, by overlapping spectrolyzer data downloads) are removed.
#' Spectra are saved as nested tibble in `spc`.
#' @import tidyverse
#' @import progress
#' @export
Spectro_batch_load<-function(parent_dir,
                             wavelengths=seq(200,750,2.5),
                             sel_wavelengths=seq(200,722.5,2.5),
                             zip=T,
                             exclude=NA,
                             read_param=FALSE){

  # check if directroy string ends with "/"; if not, add it
  if(str_ends(parent_dir,"/")==F){
    parent_dir<-paste0(parent_dir,"/")
  }

  # unzippery
  if(zip==T){
    # disregard all non zip folders (i.e. Spectro_data where unzipped folders are saved)
    zip_folders<-list.files(parent_dir,pattern=".zip")
    zip_folders<-subset(zip_folders,zip_folders%in%exclude==F)

    for (i in zip_folders){
      unzip_local(paste0(parent_dir,i),new_dir="Spectro_data")
    }

    # after unzipping, change parent_dir to new folder with unzipped data
    parent_dir<-paste0(parent_dir,"Spectro_data/")

    # might cause an issue, if the "data" folder is "contaminated"
    # with folders that were previously put there.
    # [-1] because first entry is always parent dir itself
    folders<-list.dirs(strtrim(parent_dir,nchar(parent_dir)-1))[-1]


  }else{
    # when already unzipped data:
    # [-1] because first entry is always parent dir itself
    # also weird: list.dirs wants "path/dir" but NOT "path/dir/"
    folders<-list.dirs(strtrim(parent_dir,nchar(parent_dir)-1))[-1]
    subset(folders,folders%in%exclude==F)
  }

  # initialising
  S_fp<-tibble()
  S_par<-tibble()
  print("List of folders:")
  print(folders)
  # load all data into the MOAT (mother of all tibbles (= )
  pb=progress::progress_bar$new(total = length(folders))
  for (i in folders){
    pb$tick()
    # fingerprint read-in
    if(str_detect(i,"fingerprint")){
      S_fp<-bind_rows(S_fp,Spectro_load_fingerprint(i, wavelengths = wavelengths, sel_wavelengths = sel_wavelengths, id=T))

      # parameter read-in
    }else if(str_detect(i,"parameter")&read_param==TRUE){
      S_par<-bind_rows(S_par,Spectro_load_parameter(i,id=T))
      # ERROR case
    }else if(read_param==FALSE){
      message("skipping parameter data")
    }else{
      message("ERROR Unknown folder. Only folders containing 'fingerprint' or 'parameter' designation are allowed")
    }

}


  # combining fingerprint and parameter tables
  if(read_param){
    Spectro_data<-Spectro_merge_col(S_fp,S_par)
  }else{
    Spectro_data=S_fp
  }
  # remove duplicates, which may have been caused by overlapping timeframes of teh readouts
  # which are ultimately the zip files that were read in earlier
  Spectro_data<-distinct(Spectro_data,paste(format(Date_Time,"%d.%m.%Y-%H:%M:%S"),Serial_No),.keep_all = T) #aslo creates unique column
  names(Spectro_data)[length(Spectro_data)]<-"identifier"
  return(Spectro_data)
}





