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
#' @param col The column name that the graph outputs
#' @return A bar graph
#'
#' @examples
#' top_n_plots(countries_search)

top_n_plots <- function(df,n,col){
  sntable<-sort(table(df$ScientificName),decreasing=T)
  sntable <-head(sntable,n)

  df <- subset(df, ScientificName %in% names(sntable))
  df <- within(df, ScientificName <- factor(ScientificName,levels=names(sort(table(ScientificName),decreasing=TRUE))))

  ggplot(data = df) + geom_bar(mapping = aes(x = ScientificName, fill = ScientificName)) + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

}








sntable<-sort(table(ictaluridae$ScientificName),decreasing=T)
sntable <-head(sntable,10)

ict2 <- subset(ictaluridae, ScientificName %in% names(sntable))
ict2 <- within(ict2, ScientificName <- factor(ScientificName,
                                                levels=names(sort(table(ScientificName),
                                                                  decreasing=TRUE))))

ggplot(data = ict2) + geom_bar(mapping = aes(x = ScientificName, fill = ScientificName))
                                + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

