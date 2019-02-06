#Loading the rvest package
library(rvest)
library(splitstackshape)
library(tidyr)
library(data.table)

#Specifying the url for desired website to be scraped
url <- 'https://www.imdb.com/chart/toptv/?ref_=nv_tvv_250'

#Reading the HTML code from the website
webpage <- read_html(url)

#Using CSS selectors to scrap the rankings section
rank_data_html <- html_nodes(webpage,'.imdbRating , .titleColumn , .velocity')

#Converting the ranking data to text
rank_data <- html_text(rank_data_html)

#Let's have a look at the rankings
head(rank_data)

#Data-Preprocessing: Converting rankings to numerical
#rank_data<-as.numeric(rank_data)

#Let's have another look at the rankings
#head(rank_data)

#Data-Preprocessing: removing \n
rank_data <- gsub("\n","", rank_data)

#Data-Preprocessing: removing excess spaces
rank_data<-gsub(" ","",rank_data)

#Let's have another look at the genre data
head(rank_data)

df <- data.frame(Rank = rank_data)

df <- cbind(head(df,-1), tail(df,-1))

required <- seq(1, nrow(df), 2)
df <- df[required, ]

colnames(df)[[2]] <- "imdb_rating"


df <- df %>% separate(Rank, into = c('rank', 'tv_show'), sep = 2)


df <- cSplit(df, "tv_show", "(")

setDT(df)
setnames(df, "tv_show_1", "tv_shows")
setnames(df, "tv_show_2", "year")

df[, year := gsub(")", "", year)][, rank := 1:.N][, tv_shows := as.character(tv_shows)][, tv_shows := ifelse(substr(tv_shows, 1, 1) == ".", sub("\\.", "", tv_shows), tv_shows)]

df <- df[, c("rank", "tv_shows", "imdb_rating", "year")]



df[, tv_show := ifelse(substr(tv_shows, 2, 2) == ".", gsub("^[^.]*.", "", tv_shows), tv_shows)]

###############################################################################################################

## get genre now
for (i in 1:50) {
  

#Specifying the url for desired website to be scraped
url <- paste0('https://www.imdb.com/search/keyword?keywords=tv-show&ref_=fn_kw_kw_1&sort=moviemeter,asc&mode=detail&page=', i)

#Reading the HTML code from the website
webpage <- read_html(url)

#Using CSS selectors to scrap the rankings section
shows_html <- html_nodes(webpage,'.genre , strong , .lister-item-header a')


#Converting the ranking data to text
shows <- html_text(shows_html)
#Data-Preprocessing: removing \n
shows <- shows[-seq(1:16)]
shows <- gsub("\n","", shows)

#Data-Preprocessing: removing excess spaces
shows <- gsub(" ", "", shows)

if(length(shows) == 150){


df1 <- cbind.data.frame(split(shows, rep(1:3, times = length(shows) / 3)), stringsAsFactors = F)
names(df1) <- c("tv_shows", "genre", "imdb_rating")


if(i == 1){
df2 <- copy(df1)

} else{
  df2 <- rbind(df2, df1)
}
}
}


rm(list=setdiff(ls(), c("df", "df2"))) 

df1 <- left_join(df, df2, by = "tv_shows")
