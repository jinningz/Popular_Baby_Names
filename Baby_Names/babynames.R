
## Copyright (c) [2014] [Jinning Zhang] (MIT)
## See the LICENSE file in the root directory of http://github.com/jinningz/DataAnalysis

## Author: Jinning Zhang
## Created: July 24, 2014
## Updated: July 26, 2014


## Load R packages
library(ggplot2)

## Setup working directory
setwd("E:/Data Analysis/Baby Names")

## Download and prepare data set
download.file("http://www.ssa.gov/oact/babynames/names.zip", "names.zip")
unzip("names.zip", exdir="./names")

data <- data.frame(rank = numeric(0), year = character(), name=character(), sex = character(), number = numeric(0))

for (i in 1880:2013) {
        print(i) ## It takes a while to load all data files, screen display progress
        datafile <- read.table(paste("./names/yob", i, ".txt", sep=""), sep=",", stringsAsFactors = FALSE, col.names=c("name", "sex", "number"))
        datafile <- cbind(year=i, datafile)
        rank <- c(1:nrow(datafile[datafile$sex == "F",]), 1:nrow(datafile[datafile$sex == "M",]))
        datafile <- cbind(rank, datafile)
        data <- rbind(data, datafile)
}

##head(datafile[datafile$sex == "F",])
##head(datafile[datafile$sex == "M",])


## Subsetting data
babyName <- "Bryan"
singleNameData <- data[data$name == babyName,]

## Plot ranks and number of occurances over time
qplot(year, rank, data = singleNameData , colour = sex) + scale_y_reverse()
qplot(year, number, data = singleNameData , colour = sex)

## Ranks over time
qplot(year, rank, data = singleNameData [singleNameData $sex == "M",], colour = "red") + geom_line() + scale_y_reverse()
qplot(year, rank, data = singleNameData [singleNameData $sex == "M",], colour = "red") + geom_line() + geom_smooth(method = "loess", size = 1) + scale_y_reverse()

## Number of occurances over time
qplot(year, number, data = singleNameData [singleNameData $sex == "M",], colour = "red") + geom_line()
qplot(year, number, data = singleNameData [singleNameData $sex == "M",], colour = "red") + geom_line() + geom_smooth(method = "loess", size = 1)

## The plots should be exactly the same as shown in the following websites. Filter for US data.
## http://www.babycenter.com/baby-names-bryan-766.htm
## http://www.babynamewizard.com/voyager#prefix=bryan&sw=both&exact=true

## Inspecting ranks of each gender
qplot(year, rank, data = singleNameData [singleNameData $sex == "M",], colour = "red") + geom_line() + scale_y_reverse()
qplot(year, rank, data = singleNameData [singleNameData $sex == "F",], colour = "red") + geom_line() + scale_y_reverse()



## Additinal analysis
## Year-over-year differences in rank and popularity

## Calculate yearly differences and prepare the new dataset
dataLag1 <- data[data$year != 2013, ]
dataLag1$year <- dataLag1$year + 1
## The following code take a while to calculate
mergedData <- merge(data, dataLag1, by = c("year", "name", "sex"))
## Calculate differences
## Nagative value: higher rank than previous year
## Positive value: lower rank than previous year
mergedData$rankDiff_yy <- mergedData$rank.x - mergedData$rank.y
mergedData$numberDiff_yy <- mergedData$number.x - mergedData$number.y
head(mergedData)

mergedData <- mergedData[, c(1, 2, 3, 4, 5, 8, 9)]
colnames(mergedData)[4:5] <- c("rank", "number")
head(mergedData)

## Top 30 Names
topNames <- unique(mergedData[mergedData$rank <= 30, "name"])
qplot(year, rankDiff_yy, data = mergedData[mergedData$sex == "M" & mergedData$name %in% topNames, ], colour = name) + geom_line()

## Top 30 Names in 2013 (both gender)
topNames2013 <- mergedData[mergedData$rank <= 30 & mergedData$year == 2013, "name"]; length(topNames2013)
## Changes since 2000
qplot(year, rankDiff_yy, data = mergedData[mergedData$year >= 2000 & mergedData$sex == "M" & mergedData$name %in% topNames2013, ], colour = name) + geom_line() + scale_y_reverse()

## Historical Development of the Top 5 boy Names in 2013
top5MNames2013 <- mergedData[mergedData$rank <= 5 & mergedData$year == 2013 & mergedData$sex == "M", "name"]; length(top5MNames2013)
qplot(year, rankDiff_yy, data = mergedData[mergedData$year >= 1970 & mergedData$sex == "M" & mergedData$name %in% top5MNames2013, ], colour = name) + geom_line() + scale_y_reverse()
qplot(year, rankDiff_yy, data = mergedData[mergedData$year >= 2000 & mergedData$sex == "M" & mergedData$name %in% top5MNames2013, ], colour = name) + geom_line() + scale_y_reverse()
## Liam: highest gain in rank occurred in 2009
mergedData[mergedData$name == "Liam" & mergedData$sex == "M" & mergedData$year >= 2000,]

## Difference in Ranks adjusted for actual rank value
mergedData$stdrankDiff_yy <- mergedData$rankDiff_yy / mergedData$rank

## How the top 5 boy names was progressing since 1970 and 2000
qplot(year, stdrankDiff_yy, data = mergedData[mergedData$year >= 1970 & mergedData$sex == "M" & mergedData$name %in% top5MNames2013, ], colour = name) + geom_line() + scale_y_reverse()
qplot(year, stdrankDiff_yy, data = mergedData[mergedData$year >= 2000 & mergedData$sex == "M" & mergedData$name %in% top5MNames2013, ], colour = name) + geom_line() + scale_y_reverse()

## Which baby name climb up the ranking chart the most in 2013, adjusted for actual ranks
mergedData1 <- mergedData[mergedData$year == 2013 & mergedData$sex == "M" & mergedData$rank <= 200, ]
top5M2013 <- mergedData1[order(mergedData1$stdrankDiff_yy)[1:5],]; top5M2013
top5MNames2013 <- top5M2013[, "name"]; top5MNames2013
## Ranks of these names since 2000
## Jase is the name that climbs up the ranking chart the most drastically
qplot(year, rank, data = mergedData[mergedData$sex == "M" & mergedData$year >= 2000 & mergedData$name %in% top5MNames2013, ], colour = name) + geom_line() + scale_y_reverse()


## Output the data file for Tableau visualization
mergedData_Output <- mergedData[mergedData$rank <= 500,]
write.csv(mergedData_Output, file="Baby Names Flat File.csv", row.names = F)

