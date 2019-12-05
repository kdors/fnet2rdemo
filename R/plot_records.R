
plot_records <- function(input, color = TRUE) {
  if color == TRUE{
    plot(input$ScientificName, xlab = "Scientific Name", ylab = "Number of records", main = "Records by Scientific Name", col = rainbow(nlevels(input$ScientificName)))  
  } else {
    plot(input$ScientificName, xlab = "Scientific Name", ylab = "Number of records", main = "Records by Scientific Name")
  }
}