#' Summarize a set of records downloaded from FishNet2
#'
#' Creates a simple summary of data returned by a FishNet2 search.
#'
#' @export
#' @param input Output from \code{\link{vertsearch}}, 
#'    \code{\link{searchbyterm}}, or \code{\link{spatialsearch}}. Required.
#' @param verbose Print progress and information messages. Default: TRUE
#' @return A list of summary statistics
#' @details \code{\link{vertsummary}} provides information on the sources, 
#' types and extent of data returned by a VertNet search.
#' @examples \dontrun{
#' # get occurrence records
#' recs <- vertsearch("Junco hyemalis", limit = 10)
#' 
#' # summarize occurrence records
#' vertsummary(recs)
#' 
#' vertsummary(vertsearch("Oncorhynchus clarki henshawi"))
#' }
input <- read.csv("~/Desktop/FIshNet2_Modifications/searchres.csv")

fishsummary <- function(input, verbose = TRUE) {
  # recs <- number of records in the data frame
  recs <- nrow(input)
  
  # coords <- number of records with viable lat and long data
  # errest <- number of "coords" records with viable coordinate uncertainty estimate
  if (is.null(input$Latitude) & is.null(input$Longitude)) {
    coords <- 0
  } else{ 
    coords <- NULL
  }
  
  if (is.null(input$CoordinateUncertaintyInMeters)) {
    errest <- 0
  } else {
    errest <- NULL
  }
  if (is.null(coords)) {
    coords <- sum(stats::complete.cases(input[, c('Latitude','Longitude')]))
    # checking for good lat/long data (if not, use only the above line)
    input$Latitude <- as.numeric(as.character(input$Latitude))
    input$Longitude <- as.numeric(as.character(input$Longitude))
    if (is.null(errest)) {
      input$CoordinateUncertaintyInMeters <- as.numeric(as.character(input$CoordinateUncertaintyInMeters))
    }
    mappable <- input[stats::complete.cases(input[,c('Latitude','Longitude')]),]
    #mappable <- subset(mappable, input$decimallatitude < 90 & input$decimallatitude > -90)
    #mappable <- subset(mappable, input$decimallongitude < 180 & input$decimallongitude > -180)
    if (nrow(mappable) < coords) {
      bad <- coords - nrow(mappable)
      mssg(verbose, paste(bad, " record(s) with bad coordinates"))
      coords <- coords - bad
    }
    if (is.null(errest)) {
      mappable <- subset(mappable, input$CoordinateUncertaintyInMeters > 0 &
                           input$CoordinateUncertaintyInMeters < 20020000)
      if ((errest <- nrow(mappable)) < coords) {
        bad <- coords - errest
      }
    }
  }
  
  # instcoll <- number of records from each institution+collection
  removeDups <- function(x) {
    paste(unique(unlist(strsplit(x, split = " "))), collapse = " ")
  }
  if (inherits(input$InstitutionCode, "NULL") & inherits(input$CollectionCode, "NULL")) {
    instcoll <- NA
  } else {
    instcoll <- as.matrix(paste(input$InstitutionCode, 
                                input$CollectionCode, sep = "/"))
    instcoll <- table(apply(instcoll, 1, removeDups))
  }
  
  # country <- number of records from each country
  
  if (is.null(input$Country)) {
    country <- NA 
  }  else {
    country <- c()
    # replace United States with USA for consistency
    for (c in 1:length(input$Country)) {
      if(input$Country[c] == "United States"){
        country <- c(country, "USA")
      } else{
        country <- c(country, as.character(input$Country[c]))
      }
    }
    country <- table(country)
  }
  
  # year <- number of records by year
  if (is.null(input$YearCollected)){
    year <- NA
  } else {
    year <- table(input$YearCollected)
  }
  
  # family <- number of records by family name
  if (is.null(input$Family)){
    family <- NA
  } else {
    #input$Family[input$Family==""] <- NA
    family <- table(input$Family)
  }
  
  #if (!inherits(sw(input$infraspecificepithet), "NULL")) {
   # taxon <- as.matrix(paste(taxon, input$infraspecificepithet, sep = " "))
  #}
  #taxon <- gsub(" NA", "", taxon) # remove unknowns - usually infrasp.ep
  #taxon <- table(apply(taxon, 1, removeDups))
  
  # return summary
  res = structure(list("recs" = recs, "coords" = coords, "errest" = errest, 
                 "instcoll" = instcoll, "country" = country, "year" = year, "family" = family))

print.fishsummary <- function(x, ...){
  cat(paste0("Number of records ($recs): ", x$recs), sep = "\n") 
  cat(paste("Records with decimal lat/long (-90<lat<90, -180<long<180) ($coords): ", x$coords, sep = ""), sep = "\n")
  cat(paste("Records with lat/long and coordinate uncertainty estimate (0<errest<20020000) ($errest): ", x$errest), sep = "\n")
  cat("Record count by institution/collection ($instcoll): ", sep = "\n")
  print(x$instcoll)
  cat("\nRecord count by country ($country): ", sep = "\n")
  print(x$country)
  cat("\nRecord count by year ($year): ", sep = "\n")
  print(x$year)
  cat("\nRecord count by familiy ($family): ", sep = "\n")
  print(x$family)
}

print.fishsummary(res)
return(res)
}

x <- fishsummary(input)
