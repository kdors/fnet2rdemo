#' Outputs a bar graph giving the top n in frequency in specified column of
#' dataframe
#'
#' \code{top_n_plots} returns a bar graph that shows the top n (n is given as a
#' parameter) labels in a given column in the dataframe with the highest
#' frequency
#'
#' This is a function to create and output a bar graph giving the top n in
#' frequency in specified column of dataframe (columns include 'ScientificName',
#' 'Family', 'Country','State/Province','County').
#'
#' @export
#' @param df A dataframe in FishNet2 standard format (by using read.csv())
#' @param n The number of the labels with the highest frequencies to be included
#'   in the graph
#' @param colName The column name that the graph outputs
#' @return A bar graph
#'
#' @examples
#' top_n_plots(countries_search,10,"ScientificName")

top_n_plots <- function(df,n,colName){
  #check if column exists
  if (colName %in% colnames(df) == FALSE){
    stop("Column does not exist.")
  }
  sntable<-sort(table(df[[colName]]),decreasing=T)
  sntable <-head(sntable,n)

  df <- df[df[,colName] %in% names(sntable),]
  df <- within(df, colName <- factor(colName,levels=names(sort(table(colName),decreasing=TRUE))))
  ggplot(data = df) + geom_bar(mapping = aes_string(x = colName, fill = colName)) + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

}


