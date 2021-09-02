col_ids_from_name <- function (x, sep = "_"){
  x <- gsub("[^[:alnum:]]", "_", x)
  x <- remove_accents(x)
  x <- tolower(x)
  x <- gsub("-+", "_", x)
  x <- gsub("[[:punct:]]+","_",x)
  x <- gsub("+[[:punct:]]$", "", x)
  x <- gsub("^-.", "", x)
  x


  # x <- gsub("[^[:alnum:]]", "-", x)
  # x <- remove_accents(tolower(x))
  # x <- gsub("-+", "-", x)
  # x <- gsub("^-.", "", x)
  # x

}

validated <- function(want, is){
  validation(met = TRUE)
}

not_validated <- function(want, is){
  validation(met = FALSE,
             want = want,
             is = is)
}

validation <- function(met, want, is){
  if(met){
    list(met = TRUE)
  } else {
    list(met = FALSE,
         want = want,
         is = is)
  }
}


remove_accents <- function (string) {
  accents <- "àèìòùÀÈÌÒÙáéíóúýÁÉÍÓÚÝäëïöüÄËÏÖÜâêîôûÂÊÎÔÛñÑç"
  translation <- "aeiouAEIOUaeiouyAEIOUYaeiouAEIOUaeiouAEIOUnNc"
  chartr(accents, translation, string)
}
