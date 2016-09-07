


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







