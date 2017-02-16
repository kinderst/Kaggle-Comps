library(rjson)
library(ggplot2)
library(httr)
library(EBImage)
library(gridExtra)

shotData <- read.csv("data.csv")

# unlist shot data, save into a data frame
#shotDataf <- data.frame(matrix(unlist(shotData$resultSets[[1]][[3]]), ncol=25, byrow = TRUE))

# shot data headers
#colnames(shotDataf) <- shotData$resultSets[[1]][[2]]

# covert x and y coordinates into numeric
#shotData$LOC_X <- as.numeric(as.character(shotData$LOC_X))
#shotData$LOC_Y <- as.numeric(as.character(shotData$LOC_Y))
#shotData$SHOT_DISTANCE <- as.numeric(as.character(shotDataf$SHOT_DISTANCE))

# have a look at the data
#View(shotData)

# simple plot using EVENT_TYPE to colour the dots
yearOne <- shotData[shotData$season == '2000-01',]
yearOneClean <- yearOne
yearOneClean$shot_made_flag <- as.factor(yearOneClean$shot_made_flag)
View(yearOneClean)
yearOneClean <- na.omit(yearOneClean)

ggplot(yearOneClean, aes(x=yearOneClean$loc_x, y=yearOneClean$loc_y)) +
  geom_point(aes(colour = as.factor(shot_made_flag)))

library(grid)
library(jpeg)

# half court image
courtImg.URL <- "https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg"
court <- rasterGrob(readImage(courtImg.URL), width=unit(1,"npc"), height=unit(1,"npc"))

# plot using NBA court background and colour by shot zone
ggplot(shotDataf, aes(x=LOC_X, y=LOC_Y)) + 
  annotation_custom(court, -250, 250, -50, 420) +
  geom_point(aes(colour = SHOT_ZONE_BASIC, shape = EVENT_TYPE)) +
  xlim(250, -250) +
  ylim(-50, 420) +
  coord_fixed() +
  