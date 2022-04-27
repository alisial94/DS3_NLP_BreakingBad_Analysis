######################################################################
##          DATA SCIENCE 3 - Unstructured text analysis             ##
##                                                                  ##
##   Get the transcripts for all "Breaking Bad" episodes.           ##
##   I will scrape them from                                        ## 
##   https://transcripts.foreverdreaming.org/viewforum.php?f=165    ##
##                                                                  ##
######################################################################


## Load packages
library(rvest)
library(tidyverse)
library(data.table)
library(stringr)
library(tidyr)
library(pbapply)



# Get the links for the episodes ------------------------------------------

#### Links for the 3 pages where the links for episodes can be found
pages <- paste0("https://transcripts.foreverdreaming.org/viewforum.php?f=165&start=", seq(0, 50, 25))

#### Get the links for all episodes in one table
process_one_link <- function(my_link){
  t <- read_html(my_link)
  episodes <- list()
  episodes[['name']] <- t %>% html_nodes(".topictitle") %>% html_text()
  episodes[['link']] <- t %>% html_nodes(".topictitle") %>% html_attr('href')
  return(episodes)
}

episode_links <- data.table(rbindlist(lapply(pages, process_one_link)))


#### it appears that there are few links to info pages that are not episodes  
####  the names of these links are "Updates: 4/1/22" and "Breaking Bad Transcript Index"
#### both these pages are added at the start of each new page on the website, I have decided to exclude them.
episode_links <- episode_links[name != ("Updates: 4/1/22"),]
episode_links <- episode_links[name != ("Breaking Bad Transcript Index"),]


## after removing these two unwanted links, we are left with 63 links which matches the total number of episodes in the series


# Get the transcript for all episodes -------------------------------------

# link <- episode_links$link[2]
get_transcript <- function(link) {
  # print(link)
  t <- read_html(paste0("https://transcripts.foreverdreaming.org", str_sub(link, start = 2) ))
  transcript <- t %>% html_nodes("#pagecontent p") %>% html_text()
  tinfo <- t %>% html_nodes('h2') %>% html_text()
  transcript <- str_subset(transcript, "^(?!\\[)")
  transcript <- str_subset(transcript, "^(?!\\()")
  transcript <- str_subset(transcript, "^(?!Scene)")
  transcript<- transcript[grepl(':', transcript, fixed = T)]
  textdf <- 
    rbindlist(
      lapply(transcript, function(x){
        t_piaces <- strsplit(x, ':')[[1]]
        data.table('actor' = t_piaces[1], 'text' = trimws(paste(t_piaces[2:length(t_piaces)], collapse = " " )) )
      })
    )
  textdf$season <- substring(tinfo, 0, 1)
  textdf$episode <- substring(tinfo, 3, 4)
  textdf$title <- substring(tinfo, 8,nchar(tinfo))
  return(textdf)
}

t_list <- pblapply(episode_links$link, get_transcript)
full_df <- rbindlist(t_list, fill = T)


write.csv(full_df, "~/DS3_NLP_BreakingBad_Analysis/Data/BB_data.csv", row.names = F)
