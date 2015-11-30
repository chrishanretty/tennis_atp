###
### Libraries
###

library(dplyr)
library(ascii)

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

ps <- read.csv("2015_postscript.csv", as.is = TRUE)
ps$year <- 2015
dat <- merge(dat, ps, all = T)

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

### All the rest is output
### First, a dotchart

with(tail(winners, 15),
	dotchart(PlayerContrib,
	labels = paste0(winner_name, " (",winner_ioc,", ", year, ")"),
	pch = 19))

### Second, a table
### in pandoc-flavoured markdown
winners <- winners[order(winners$PlayerContrib * -1),]
plot.df <- head(winners, 12)

plot.df$Team <- paste0(plot.df$winner_ioc, " (", plot.df$year, ")")
plot.df <- plot.df[,c("Team","NatPtsWon","winner_name","PlayerPtsWon","PlayerContrib")]
plot.df$PlayerContrib <- plot.df$PlayerContrib * 100

print(ascii(plot.df, 
	digits = 0,
	colnames = c("Team","Singles points won (team)",
	"Player","Singles points won (player)","Contribution (%)")), 
	type = "pandoc", digits = 0)
