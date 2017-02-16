library(rjson)
library(ggplot2)
library(httr)
library(EBImage)
library(gridExtra)
library(hexbin)

shotData <- read.csv("data.csv")

# simple plot using EVENT_TYPE to colour the dots
yearOne <- shotData[shotData$season == '2000-01',]
yearOneClean <- yearOne
yearOneClean$shot_made_flag <- as.factor(yearOneClean$shot_made_flag)
View(yearOneClean)
yearOneClean <- na.omit(yearOneClean)
shotData <- na.omit(shotData)

fourthQtr <- shotData[shotData$minutes_remaining <= 2  & shotData$period == 4,]
overtime <- shotData[shotData$period > 4,]
clutchTime <- rbind(fourthQtr, overtime)
finalSecondsClutchTime <- clutchTime[clutchTime$seconds_remaining < 10 & clutchTime$minutes_remaining == 0,]
View(finalSecondsClutchTime)

courtImg.URL <- "https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg"
court <- rasterGrob(readImage(courtImg.URL), width=unit(1,"npc"), height=unit(1,"npc"))

ggplot(clutchTime, aes(x=clutchTime$loc_x, y=clutchTime$loc_y)) +
  annotation_custom(court, -250, 250, -50, 420) +
  geom_point(aes(colour = as.factor(shot_made_flag)))

ggplot(finalSecondsClutchTime, aes(x=finalSecondsClutchTime$loc_x, y=finalSecondsClutchTime$loc_y)) +
  annotation_custom(court, -250, 250, -50, 420) +
  geom_point(aes(colour = as.factor(shot_made_flag))) +
  xlim(250, -250) +
  ylim(-50, 420)

fsctThree <- finalSecondsClutchTime[finalSecondsClutchTime$shot_type == '3PT Field Goal',]
View(fsctThree)
ggplot(fsctThree, aes(x=fsctThree$loc_x, y=fsctThree$loc_y)) +
  annotation_custom(court, -250, 250, -50, 420) +
  geom_point(aes(colour = as.factor(shot_made_flag))) +
  xlim(250, -250) +
  ylim(-50, 420)
  

library(grid)
library(jpeg)

# half court image


# plot using NBA court background and colour by shot zone
ggplot(shotDataf, aes(x=LOC_X, y=LOC_Y)) + 
  annotation_custom(court, -250, 250, -50, 420) +
  geom_point(aes(colour = SHOT_ZONE_BASIC, shape = EVENT_TYPE)) +
  xlim(250, -250) +
  ylim(-50, 420) +
  coord_fixed()


#hex
ggplot(clutchTime, aes(x=loc_x, y=loc_y)) + 
  annotation_custom(court, -250, 250, -50, 420) +
  stat_binhex(bins = 5, colour = "gray", alpha = 0.8) +
  scale_fill_gradientn(colours = c("yellow","orange","red")) +
  guides(alpha = FALSE, size = FALSE) +
  xlim(250, -250) +
  ylim(-50, 420) +
  geom_rug(alpha = 0.2) +
  coord_fixed()
  