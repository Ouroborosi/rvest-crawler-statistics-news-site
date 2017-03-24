# @author Sebastian 
# @version 2016/10/30
# 
# Desc: use R(3.3.1) to get info data from saverial news site

# import
library("rvest")

# declare cons
crawler.date <- NULL 		# scan date
crawler.webs <- NULL 		# scan website
data.temp_content <- NULL 	# saving the data from each web
data.content <- NULL 		# result data list
var.date_start <- NULL 		# date begin length in string
var.date_end <- NULL 		# date end length in string
var.url <- NULL         # target website url
var.url_pattern <- NULL 	# http get parameter
var.page.source <- NULL  # page source by read_html()

# declaration function
# change URI for http get request
# @param - current page
get_Next_Page <- function(arg.pages){
  # next page
  arg.pages <- arg.pages+1
  # http get URI
  url <- paste(var.url, var.url_pattern, arg.pages, sep="")
  page.source <- read_html(url)
  
  return (list(source=page.source, pages=arg.pages))
}

# compare data date is match setting date
# @param - arg.setting.date: constant crawler.date
# @param - arg.content: data content
# @return - Boolean
matchDate <- function(arg.setting.date, arg.content){
  isMatch = F
  #parser new date from data
  start <- if(var.date_start==1){var.date_start}else{nchar(arg.content)+var.date_start}
  end <- if(var.date_end>0){var.date_end}else{nchar(arg.content)+var.date_end}
  newsDate <-  substr(arg.content, start, end)
  
  if(arg.setting.date==newsDate){
    isMatch = T
  }
  
  return (isMatch)
}

# get select data by css
# @param - arg.page_source: html source by read_html()
# @param - arg.css: selected css
# @param - arg.pages: website current page
# @return - source data
get_data_content <- function(arg.page_source, arg.css, arg.pages, arg.attr.css){
  data.node <- html_nodes(arg.page_source, arg.css)
  data.text <- html_text(data.node)
  
  data.text <- cbind(data.text, type=get_attr_content(arg.page_source, arg.attr.css))
  
  
  if(matchDate(as.character(Sys.Date()), tail(data.text[1], n=1))){
    arg.pages <- if(is.null(arg.pages)){2}else{arg.pages}
    arg.page_info <- get_Next_Page(arg.pages)
    
    data.text <- rbind(data=data.text, get_data_content(arg.page_info$source, arg.css, arg.page_info$pages, arg.attr.css))
  }
  return(data.text)
}

# get attribute val
# @param - arg.page_source: html source by read_html()
# @param - arg.css: [1] selected css [2] attribute name
# @return - attribute content
get_attr_content <- function(arg.page_source, source.attr.css){
  data.attr <- html_nodes(arg.page_source, source.attr.css[1])  %>%
    html_attr(source.attr.css[2])
  
  return(data.attr)
}

# exchange tag num to type name
# @param - tag num
# @return - title
ltnTitle <- function(arg.tag){
  matchingTable <- cbind(
    c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14), 
    c("焦點", "政治", "社會", "生活", "國際", "言論", "財金", "體育", "", "娛樂", "", "", "3C", "汽車")
    )
  
  return (matchingTable[as.numeric(arg.tag),][2])
}

# MAIN
# ----- init start -----
# setting vals for data scan
crawler.date <- Sys.Date()
crawler.webs <- NULL
# ----- init end -----

# ----- get sysdate news data start -----
# init vars
source.css <- NULL # selected css
source.attr.css <- NULL #selected attribute css
# from ltn
source.css <- ".lipic"
source.attr.css <- c(".lipic span a", "class")

var.url <- "http://news.ltn.com.tw/list/BreakingNews"
var.url_pattern <- "/?page="
var.page.source <- read_html(var.url)
var.date_start <- 1
var.date_end <- 10

data.temp_content <- get_data_content(var.page.source, source.css, NULL, source.attr.css)

data.length <- nrow(data.temp_content)
data.temp <- data.frame(date=character(), title=character(), type=numeric())
for(i in c(1:data.length)){
  current.data <- data.temp_content[i,][[1]]
  current.type <- data.temp_content[i,][[2]]
  #get current date data
  if(matchDate(as.character(Sys.Date()), substr(current.data, 1, 10))){
    data.temp <-  rbind(data.temp, data.frame(date=substr(current.data, 1, 16), title=substr(current.data, 17, nchar(current.data)), type=ltnTitle(substr(current.type, 4, nchar(current.type)))))
  }
}

data.content <- data.frame(data.temp)
View(data.content)
write.csv(result, file="ltn_news.csv", sep=",")

# ----- get sysdate news data end -----

