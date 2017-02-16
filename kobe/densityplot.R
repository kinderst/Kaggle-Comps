library(rjson)
library(ggplot2)
library(httr)
library(EBImage)
library(gridExtra)
library(hexbin)
library(grid)
library(jpeg)
library(plyr)
library(ggmap)

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


library(MASS)
library(magr)
y <- 0
inferno_colors = c('#000004', '#010107', '#02020C', '#030312', '#050417', '#07051D', '#0A0723', '#0D0829', '#100A2F', '#140B35', '#170C3B', '#1B0C41', '#1F0C48', '#230C4E', '#280B53', '#2C0B58', '#310A5D', '#350960', '#3A0963', '#3E0966', '#430A68', '#470B6A', '#4B0C6B', '#4F0D6C', '#540F6D', '#58106E', '#5C126E', '#60136E', '#64156E', '#68166E', '#6C186E', '#70196E', '#741B6E', '#781C6D', '#7D1E6D', '#811F6C', '#85216B', '#89226A', '#8D2369', '#912568', '#952667', '#992865', '#9D2964', '#A12B62', '#A52D60', '#A92E5E', '#AD305C', '#B1325A', '#B53458', '#B93656', '#BD3853', '#C03A51', '#C43C4E', '#C83F4C', '#CB4149', '#CF4446', '#D24644', '#D54941', '#D84C3E', '#DB4F3B', '#DE5338', '#E15635', '#E45A32', '#E65D2F', '#E9612B', '#EB6528', '#ED6925', '#EF6D22', '#F1711E', '#F3751B', '#F47A18', '#F67E14', '#F78311', '#F8870E', '#F98C0A', '#FA9008', '#FB9506', '#FB9A06', '#FC9F07', '#FCA409', '#FCA80D', '#FCAD12', '#FCB217', '#FBB71C', '#FBBC22', '#FAC128', '#F9C72E', '#F8CC35', '#F7D13C', '#F6D643', '#F5DB4B', '#F4E054', '#F3E45D', '#F2E967', '#F1EE71', '#F2F27C', '#F3F587', '#F5F991', '#F8FC9B', '#FCFFA4')
circle_points = function(center = c(0, 0), radius = 1, npoints = 360) {
  angles = seq(0, 2 * pi, length.out = npoints)
  return(data.frame(x = center[1] + radius * cos(angles),
                    y = center[2] + radius * sin(angles)))
}

theme_court = function(base_size = 16) {
  theme_bw(base_size) +
    theme(
      text = element_text(color = "#f0f0f0"),
      plot.background = element_rect(fill = bg_color, color = bg_color),
      panel.background = element_rect(fill = bg_color, color = bg_color),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks.length = unit(0, "lines"),
      legend.background = element_rect(fill = bg_color, color = bg_color),
      legend.position = "bottom",
      legend.key = element_blank(),
      legend.text = element_text(size = rel(1.0))
    )
}

width = 50
height = 94 / 2
key_height = 19
inner_key_width = 12
outer_key_width = 16
backboard_width = 6
backboard_offset = 4
neck_length = 0.5
hoop_radius = 0.75
hoop_center_y = backboard_offset + neck_length + hoop_radius
three_point_radius = 23.75
three_point_side_radius = 22
three_point_side_height = 14

short_three_radius = 22
short_three_seasons = c("1994-95", "1995-96", "1996-97")

court_points = data.frame(
  x = c(width / 2, width / 2, -width / 2, -width / 2, width / 2),
  y = c(height, 0, 0, height, height),
  desc = "perimeter"
)

court_points = rbind(court_points , data.frame(
  x = c(outer_key_width / 2, outer_key_width / 2, -outer_key_width / 2, -outer_key_width / 2),
  y = c(0, key_height, key_height, 0),
  desc = "outer_key"
))

court_points = rbind(court_points , data.frame(
  x = c(-backboard_width / 2, backboard_width / 2),
  y = c(backboard_offset, backboard_offset),
  desc = "backboard"
))

court_points = rbind(court_points , data.frame(
  x = c(0, 0), y = c(backboard_offset, backboard_offset + neck_length), desc = "neck"
))

foul_circle = circle_points(center = c(0, key_height), radius = inner_key_width / 2)
foul_circle_top = filter(foul_circle, y > key_height) %>% mutate(desc = "foul_circle_top")
foul_circle_bottom = filter(foul_circle, y < key_height) %>% mutate(desc = "foul_circle_bottom")

hoop = circle_points(center = c(0, hoop_center_y), radius = hoop_radius) %>% mutate(desc = "hoop")

restricted = circle_points(center = c(0, hoop_center_y), radius = 4) %>%
  filter(y >= hoop_center_y) %>%
  mutate(desc = "restricted")

three_point_circle = circle_points(center = c(0, hoop_center_y), radius = three_point_radius) %>% filter(y >= three_point_side_height)
short_three_circle = circle_points(center = c(0, hoop_center_y), radius = short_three_radius) %>% filter(y >= hoop_center_y)

three_point_line = data.frame(
  x = c(three_point_side_radius, three_point_side_radius, three_point_circle$x, -three_point_side_radius, -three_point_side_radius),
  y = c(0, three_point_side_height, three_point_circle$y, three_point_side_height, 0),
  desc = "three_point_line"
)

short_three_line = data.frame(
  x = c(three_point_side_radius, three_point_side_radius, short_three_circle$x, -three_point_side_radius, -three_point_side_radius),
  y = c(0, hoop_center_y, short_three_circle$y, hoop_center_y, 0),
  desc = "short_three_line"
)

court_without_three = rbind(court_points , foul_circle_top, foul_circle_bottom, hoop, restricted)

court_points = rbind(court_without_three, three_point_line)
court_points = mutate(court_points , dash = (desc == "foul_circle_bottom"))

m <- ggplot(fsctThree, aes(x=fsctThree$loc_x, y=fsctThree$loc_y)) +
  annotation_custom(court, -250, 250, -50, 420) +
  geom_point(aes(colour = as.factor(shot_made_flag))) +
  xlim(250, -250) +
  ylim(-50, 420)
m + stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE) +
  geom_path(data = court_points,
            aes(x = x, y = y, group = desc, linetype = dash),
            color = "#999999") +
  scale_fill_gradientn(colors = inferno_colors, guide = FALSE ) +
  scale_colour_gradientn("Shot frequency    ",
                         limits = c(0, 1),
                         breaks = c(0, 1),
                         labels = c("lower", "higher"),
                         colours = inferno_colors,
                         guide = guide_colorbar(barwidth = 15)) +
  theme(legend.text = element_text(size = rel(0.6)))

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
