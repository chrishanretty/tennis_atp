###
### Libraries
###

library(dplyr)
library(ggplot2)

### 
### Data
###

filelist <- list.files(pattern = ".*matches.*.csv")
dat <- list()
for (f in filelist) {
	tmp <- read.csv(f,as.is = TRUE)
	tmp$year <- as.numeric(gsub("\\D","",f))
	tmp <- subset(tmp, grepl("Davis Cup", tmp$tourney_name))
	dat[[f]] <- tmp
}
dat <- do.call("rbind", dat)

### Within each year, calculate total number of points won by each national team
country.df <- dat %>% 
	group_by(year, winner_ioc) %>%
	summarize(NatPtsWon = length(winner_ioc))
### Within each year, calculate total number of points won by each player
player.df <- dat %>% 
	group_by(year, winner_ioc, winner_name, winner_id) %>%
	summarize(PlayerPtsWon = length(winner_name))
### Merge these two
dat <- merge(country.df, player.df, all = T)
dat$PlayerContrib <- dat$PlayerPtsWon / dat$NatPtsWon

### Just select those countries that won the final
winners <- read.csv("cup_winners.csv")
winners <- merge(winners, dat, 
	all.x = T,
	all.y = F)
winners <- subset(winners, !is.na(NatPtsWon))

### Within the winners, select the player who contributed most
winners <- winners %>%
	group_by(year, winner_ioc) %>% 
	filter(PlayerContrib == max(PlayerContrib))
winners <- winners[order(winners$PlayerContrib),]

with(tail(winners, 15),
	dotchart(PlayerContrib,
	labels = paste0(winner_name, " (",winner_ioc,", ", year, ")"),
	pch = 19))
