# Manipulate southern oscillation index from BOM
library(data.table)

mydata2 <- read.csv("Southern Oscillation Index.csv", header = T)

mydata4 <- as.data.table(mydata2)

mydata3 <- melt.data.table(mydata4, id.vars = "Year", variable.name = "Month", value.name = "SOI")
mydata5 <- mydata3
mydata5$Month <- match(mydata5$Month, month.abb)

fwrite(mydata5 , "SOI data long.csv")
