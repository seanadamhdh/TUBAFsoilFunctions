
#' read Pangaea delimited text data with header
#' 
#' 
#' 
#' @param path Character string with path to Pangaea .tab-file
#' @param dec_sep decimal separator (default=".")
#' @param col_sep column separator (default="\t")
#' @param skiplines description lines to be skipped. Can be assinged manual by setting integer number, or by default is "auto",
#' looking for the end of the description marked by "*/"
#' @param shortnames SHould only abbreviations be returned as colnames or full descriptive titles? If True (default) only short names (abbreviations) are returned /used.
#' Cropping first ".." of colnames.
#' @importFrom stringr str_split_fixed
#' @export
readPangaea=function(path,dec_sep=".",col_sep="\t",skiplines="auto",shortnames="T"){
  
  if(skiplines=="auto"){
    skiplines=which(readLines(path)=="*/")
  }
  
  out=read.table(path,
             dec=dec_sep,
             sep=col_sep,
             header = T,
             skip = skiplines)
  
  if(shortnames){
    names(out)=str_split_fixed(names(out),"\\.\\.",2)[,1]
  }
  
  
  return(out)
  
}

