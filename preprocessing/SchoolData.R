library("ggplot2")
library("readr")
library("haven")

raceData <- read_csv("data/raceData/raceData.csv",col_types = c(col_double()))
schoolInfo <- read_csv("data/schoolInfo/schoolInfo.csv",col_types = cols(LEAID=col_double()))
zipLookup <- read_csv("data/zipLookup/zipLookup.csv",col_types = cols(col_double(),col_double()))


calcFrac <- function(num, none, total) {
  return ((num / (1 - none)) / total)
}


diversity <- function(data) {
  TotalReported <-
    data[data$TOTAL_INDICATOR == 1, ]$STUDENT_COUNT
  DerivedValues <-
    data[data$TOTAL_INDICATOR == 2, ]
  FractionNoneReported <-
    DerivedValues[DerivedValues$RACE_ETHNICITY == 1, ]$STUDENT_COUNT / TotalReported
  FractionAmIndian <-
    calcFrac(
      sum(DerivedValues[DerivedValues$RACE_ETHNICITY == 3, ]$STUDENT_COUNT, na.rm =
            TRUE),
      FractionNoneReported,
      TotalReported
    )
  FractionAsian <-
    calcFrac(
      sum(DerivedValues[DerivedValues$RACE_ETHNICITY == 4, ]$STUDENT_COUNT, na.rm =
            TRUE),
      FractionNoneReported,
      TotalReported
    )
  FractionAfricanAm <-
    calcFrac(
      sum(DerivedValues[DerivedValues$RACE_ETHNICITY == 5, ]$STUDENT_COUNT, na.rm =
            TRUE),
      FractionNoneReported,
      TotalReported
    )
  FractionHispanic <-
    calcFrac(
      sum(DerivedValues[DerivedValues$RACE_ETHNICITY == 6, ]$STUDENT_COUNT, na.rm =
            TRUE),
      FractionNoneReported,
      TotalReported
    )
  FractionPacIslander <-
    calcFrac(
      sum(DerivedValues[DerivedValues$RACE_ETHNICITY == 7, ]$STUDENT_COUNT, na.rm =
            TRUE),
      FractionNoneReported,
      TotalReported
    )
  
  FractionWhite <-
    calcFrac(
      sum(DerivedValues[DerivedValues$RACE_ETHNICITY == 8, ]$STUDENT_COUNT, na.rm =
            TRUE),
      FractionNoneReported,
      TotalReported
    )
  FractionTwoOrMore <-
    calcFrac(
      sum(DerivedValues[DerivedValues$RACE_ETHNICITY == 9, ]$STUDENT_COUNT, na.rm =
            TRUE),
      FractionNoneReported,
      TotalReported
    )
  
  Distribution <-
    sqrt(((FractionAfricanAm - (1 / 7)) ^ 2) +     ((FractionAmIndian - (1 / 7)) ^ 2) +
           ((FractionAsian - (1 / 7)) ^ 2) +       ((FractionHispanic - (1 / 7)) ^ 2) +
           ((FractionPacIslander - (1 / 7)) ^ 2) + ((FractionWhite - (1 / 7)) ^ 2) +
           ((FractionTwoOrMore - (1 / 7)) ^ 2))
  C1 <- 100
  C2 <- -100 * sqrt(42) / 6
  return (C1 + (C2 * Distribution))
}
ediList <- integer(18790)
fipsList <- integer(18790)

uniqueElems <- unique(raceData$LEAID)

for (i in 1:length(uniqueElems)) {
  data <- raceData[raceData$LEAID == uniqueElems[i] , ]
  
  ediList[i]  <- diversity(data)
  zipCode <- schoolInfo[schoolInfo$LEAID==uniqueElems[i],]$MZIP
  search <- zipLookup[zipLookup$ZIP==zipCode,]
  fipsList[i] <- ifelse(nrow(search)==0, -1, search$COUNTY)
}




ediDF <- data.frame(LEAID = uniqueElems, FIPS=fipsList, EDI = ediList)
