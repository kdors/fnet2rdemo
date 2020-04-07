#' Heat Map of Occurrence Frequency by Country
#'
#' Creates a heatmap of the frequency of an occurrence by country/region.
#'
#' @export
#' @param df A dataframe in FishNet2 standard format with column labeled 'Country'
#' @return heatmap showing frecuency by country
#'
#' @examples
#' hearmap_world(ictaluridae)
#'
heatmap_world <- function(df){
  # check for 'Country' column existence

  # check that level is either species, genus, or family

  # check whether name corresponds to level

  # select rows with name only and check if empty

  # map country names
  info <- df %>% count(Country)
  info <- rename(info, Countries = n)
  map_info <- joinCountryData2Map(dF=info, joinCode = "NAME", nameJoinColumn = 'Country', verbose=TRUE)

  # plot map
  par(mai=c(0,0,0.2,0), xaxs="i",yaxs="i")
  mapCountryData(map_info, nameColumnToPlot = "Countries",catMethod="categorical")

}
