#' Filter a set of records downloaded from FishNet2 by Tissue column
#'
#' Filters data returned by a FishNet2 search for records that include tissue information.
#'
#' @export
#' @param input A dataframe in FishNet2 standard format (by using read.csv())
#' @param verbose Print progress and information messages. Default: TRUE
#' @return Filtered dataset with records that do not have a blank tissue value
#'
#' # summarize occurrence records
<<<<<<< HEAD
#'
hasTissues <- function(input, verbose = TRUE){
  output <- subset(input, is.na(Tissues) == FALSE)
  total <- length(output$Tissues)
=======
#' @examples
#' has_tissue(louisiana,TRUE)
#'
has_tissue <- function(input, verbose = TRUE){
  output <- input$Tissues[!is.na(input$Tissues)]
  total <- length(output)
>>>>>>> e3d5e204efec314f3da1163056083bec6761d5e9
  if(verbose == TRUE){
    print("Records with tissues: ")
    print(total)
  }
  return(output)
}


