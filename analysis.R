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
	tmp$level <- gsub(":.*","",tmp$tourney_name)
	### Note the following levels need not be exact
	### We only need to identify Cup winners (i.e., max value)
	### Note Challenge Round (CR) > Final, where "CR" exists
	tmp$level <- factor(tmp$level,
		levels = c("Davis Cup AFR R1","Davis Cup AFR QF",
			"Davis Cup AFR SF","Davis Cup AME R1",
			"Davis Cup AME PR","Davis Cup AME QF",
			"Davis Cup AME SF","Davis Cup AME F",
			"Davis Cup EUR PQ","Davis Cup EUR PR",
			"Davis Cup EUR QF","Davis Cup EUR R1",
			"Davis Cup EUR SF","Davis Cup EUR F",
			"Davis Cup G1 R1","Davis Cup G1 R2",
			"Davis Cup G1 R3","Davis Cup G1 PO",
			"Davis Cup G1 PR","Davis Cup G1 QF",
			"Davis Cup G1 SF","Davis Cup G1 F",
			"Davis Cup G2 PO","Davis Cup G2 PQ",
			"Davis Cup G2 PR","Davis Cup G2 R1",
			"Davis Cup G2 R2","Davis Cup G2 R3",
			"Davis Cup G2 QF","Davis Cup G2 SF",
			"Davis Cup G2 F","Davis Cup SAM PQ",
			"Davis Cup SAM PR","Davis Cup SAM QF",
			"Davis Cup SAM SF","Davis Cup SAM F",
			"Davis Cup WG PO","Davis Cup WG PQ",
			"Davis Cup WG PR","Davis Cup WG R1","Davis Cup WG QF",
			"Davis Cup WG SF",
			"Davis Cup WG F","Davis Cup WG CR"),
	ordered = TRUE)
	dat[[f]] <- tmp
}
dat <- do.call("rbind", dat)
dat <- subset(dat, year >= 1981)
### Within each year, calculate total number of points won by each national team
country.df <- dat %>% 
	group_by(year, winner_ioc) %>%
	summarize(NatPtsWon = length(winner_ioc),
		BestLevel = max(level))
### Within each year, calculate total number of points won by each player
player.df <- dat %>% 
	group_by(year, winner_ioc, winner_name, winner_id) %>%
	summarize(PlayerPtsWon = length(winner_name))
### Merge these two
dat2 <- merge(country.df, player.df, all = T)
dat2$PlayerContrib <- dat2$PlayerPtsWon / dat2$NatPtsWon

### Just select those countries that won the final

