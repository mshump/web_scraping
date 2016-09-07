
#install.packages("XML")
library(XML)


###Scrape for a single date ###

day_inc <- 1
dt_txt <- "2013-12-06"
dt_date <- as.Date(dt_txt)
dt_set <- as.POSIXlt(dt_date)

dt <- as.POSIXlt(dt_set + (60*60*24)*day_inc)

dt_txt <- as.character(dt)
dt_year <- dt$year + 1900
dt_month <- dt$mon + 1
dt_day <- dt$mday 


fileUrl<- paste0("http://www.cbssports.com/collegebasketball/scoreboard/div1/",dt_year,paste( if(dt_month <10){paste0("0",dt_month)}else{dt_month}),paste( if(dt_day <10){paste0("0",dt_day)}else{dt_day}))
#fileUrl<- "http://www.cbssports.com/collegebasketball/scoreboard/div1/20141116"
doc <- htmlTreeParse(fileUrl,useInternal=TRUE)

final_scores <- xpathSApply(doc, "//td[@class='finalScore']", xmlValue)
final_status <- xpathSApply(doc, "//td[@class='finalStatus']", xmlValue)
game_info <- xpathSApply(doc, "//td[@class='gameInfo']", xmlValue)
team_name <- xpathSApply(doc, "//td[@class='teamName']", xmlValue)


team_name_h <- team_name[seq(1, length(team_name) , by=2)]
team_name_a <- team_name[seq(2, length(team_name) , by=2)]
game_dt <- rep(dt_txt, length(team_name_h))

if (length(game_info) == 0){
final_scores_h <- final_scores[seq(1, length(final_scores) , by=2)]
final_scores_a <- final_scores[seq(2, length(final_scores) , by=2)]
}

if (length(game_info) > 0){

final_scores_h <- c(final_scores[seq(1, length(final_scores) , by=2)], rep(0,length(game_info)))
final_scores_a <- c(final_scores[seq(2, length(final_scores) , by=2)], rep(0,length(game_info)))
final_status <- c(final_status, game_info)
}


collect_scores <- data.frame(cbind(game_dt,team_name_h, team_name_a, final_scores_h, final_scores_a,final_status))


###Scrape for a season ###

day_lp <- 367

for(i  in 1:day_lp){


day_inc <- i-1
dt_txt <- "2013-11-14"
dt_date <- as.Date(dt_txt)
dt_set <- as.POSIXlt(dt_date)

dt <- as.POSIXlt(dt_set + (60*60*24)*day_inc)

dt_txt <- as.character(dt)
dt_year <- dt$year + 1900
dt_month <- dt$mon + 1
dt_day <- dt$mday 


fileUrl<- paste0("http://www.cbssports.com/collegebasketball/scoreboard/div1/",dt_year,paste( if(dt_month <10){paste0("0",dt_month)}else{dt_month}),paste( if(dt_day <10){paste0("0",dt_day)}else{dt_day}))
#fileUrl<- "http://www.cbssports.com/collegebasketball/scoreboard/div1/20141116"
doc <- htmlTreeParse(fileUrl,useInternal=TRUE)



final_scores <- xpathSApply(doc, "//td[@class='finalScore']", xmlValue)
final_status <- xpathSApply(doc, "//td[@class='finalStatus']", xmlValue)
game_info <- xpathSApply(doc, "//td[@class='gameInfo']", xmlValue)
team_name <- xpathSApply(doc, "//td[@class='teamName']", xmlValue)

if (length(final_scores) >0){

team_name_h <- team_name[seq(1, length(team_name) , by=2)]
team_name_a <- team_name[seq(2, length(team_name) , by=2)]
game_dt <- rep(dt_txt, length(team_name_h))
        
        if (length(game_info) == 0){
                final_scores_h <- final_scores[seq(1, length(final_scores) , by=2)]
                final_scores_a <- final_scores[seq(2, length(final_scores) , by=2)]
        }
        
        if (length(game_info) > 0){
                
                final_scores_h <- c(final_scores[seq(1, length(final_scores) , by=2)], rep(0,length(game_info)))
                final_scores_a <- c(final_scores[seq(2, length(final_scores) , by=2)], rep(0,length(game_info)))
                final_status <- c(final_status, game_info)
        }


collect_scores_tmp <- data.frame(cbind(game_dt,team_name_h, team_name_a, final_scores_h, final_scores_a,final_status))
        
        if (day_inc == 0){
                collect_scores <-  collect_scores_tmp
        }
        
        if (day_inc > 0){
                collect_scores <- rbind(collect_scores,collect_scores_tmp)
        }
}


}

collect_scores$game_dt  <- strptime(collect_scores$game_dt , "%Y-%m-%d")
head(collect_scores)
tail(collect_scores)

str(collect_scores)


setwd("C:/Users/mshump/Desktop/Professional Matt/Projects/NCAA BBALL/")
write.csv(collect_scores, file = "collect_scores.csv")



########Another site 1 day ###
day_inc <- 1
dt_txt <- "2014-11-14"
dt_date <- as.Date(dt_txt)
dt_set <- as.POSIXlt(dt_date)

dt <- as.POSIXlt(dt_set + (60*60*24)*day_inc)

dt_txt <- as.character(dt)
dt_year <- dt$year + 1900
dt_month <- dt$mon + 1
dt_day <- dt$mday 

fileUrl<- paste0("http://www.ncaa.com/scoreboard/basketball-men/d1/",dt_year,"/",dt_month,"/",dt_day)

#fileUrl<- "http://www.ncaa.com/scoreboard/basketball-men/d1/2014/11/14"
doc <- htmlTreeParse(fileUrl,useInternal=TRUE)

final_scores <- xpathSApply(doc, "//td[@class='final score']", xmlValue)
teams <- xpathSApply(doc, "//div[@class='team']", xmlValue)

final_scores_h <- final_scores[seq(1, length(final_scores) , by=2)]
final_scores_a <- final_scores[seq(2, length(final_scores) , by=2)]
teams_h <- teams[seq(1, length(teams) , by=2)]
teams_a <- teams[seq(2, length(teams) , by=2)]
game_dt <- rep(dt_txt, length(final_scores_h))


collect_scores_2 <- data.frame(cbind(game_dt, teams_h, teams_a, final_scores_h, final_scores_a))

str(collect_scores_2)


####get some golf stats ##########


site_num <- 10

site_meta <- matrix("NA", site_num,5)
dimnames(site_meta) <- list(1:site_num,c("ref","event_id", "site_info", "dt", "sof"))

event_id <- 5522 #5531

for(i  in 1:site_num){
        
        event_id <- event_id - 1 + i
        fileUrl <-paste0("http://www.owgr.com/en/Events/EventResult.aspx?eventid=",event_id)    
        doc <- htmlTreeParse(fileUrl,useInternal=TRUE)
        
        # Tournament site + other random info
        site_info <- xpathSApply(doc, "//h2", xmlValue)
        site_info <- site_info[1]
        
        # Date and strength of field
        dt_sof <- xpathSApply(doc, "//span[@class='sub_header']", xmlValue)
        dt_sof <- strsplit(dt_sof, " - ")
        dt <- dt_sof[[1]][1]
        sof <- sub("Strength of Field  ", "", dt_sof[[1]][2])
        
        site_meta[i,] <- c(i, event_id, site_info, dt, sof)
        
        
}
site_meta


#library(XML)
theurl <- "http://www.owgr.com/en/Events/EventResult.aspx?eventid=5531"
tables <- readHTMLTable(theurl)
n.rows <- unlist(lapply(tables, function(t) dim(t)[2]))
#the picked table is the longest one on the page

round_scores <- tables[[which.max(n.rows)]]
round_scores <- round_scores[-1,] 
#round_scores[1,] 
#names(round_scores)
#head(round_scores)

round_scores # table of players and round scores







