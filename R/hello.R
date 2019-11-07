library(curl)
mydata <- readLines(curl("http://www.fishnet2.net/search.aspx?t=ichthy&l=mexico%2cflorida"))
