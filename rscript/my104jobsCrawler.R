#' Jobs recommended by my 104

setwd("workspace/datascience/r/JobsInfoMining")

library(RSelenium)
library(rvest)

user_id <- readline(prompt = 'user id : ')
psw     <- readline(prompt = 'password: ')

remDr <- remoteDriver(browserName = 'chrome',
                      remoteServerAddr = '192.168.99.100',
                      port = 4445L)
remDr$open(silent = TRUE)
remDr$navigate('https://login.104.com.tw/login.cfm?frombar=jbbar_login')

web_elem <- remDr$findElement(using = 'css',
                  value = '#id_name')
web_elem$sendKeysToElement(list(user_id))
web_elem <- remDr$findElement(using = 'css',
                              value = '#password')
web_elem$sendKeysToElement(list(psw))
web_elem <- remDr$findElement(using = 'xpath',
                              value = '//*[@id="wrapper"]/div[3]/div[4]/ul/li[1]/input')
web_elem$clickElement()

#
web_elem <- remDr$findElement(using = 'css',
                              value = '#indexMatchCount_0')
web_elem$clickElement()

#
web_elem <- remDr$findElement(using = 'css',
                              value = 'body')
for( i in 1:20){
  web_elem$sendKeysToElement(list(key = "end"))
  Sys.sleep(runif(1, 0.5, 1.5))
}

page_source <- remDr$getPageSource()
html_source <- read_html(page_source[[1]])

pages <- html_source %>% html_nodes("#box_page_bottom") %>%  html_text()
pages <- pages %>% strsplit(., '\t')
pages <- pages[[1]][pages[[1]]!='']
pages <- max(as.numeric(pages), na.rm = TRUE)

for (i in 11:(pages-1)){
  web_elem <- remDr$findElement(using = 'xpath',
                                value = paste0('//*[@id="loadclick_',
                                               i,
                                               '"]/li[1]/a'))
  web_elem$clickElement()
  web_elem <- remDr$findElement(using = 'css',
                                value = 'body')
  web_elem$sendKeysToElement(list(key = "end"))
  Sys.sleep(runif(1, 0.75, 1.75))
}

page_source <- remDr$getPageSource()
html_source <- read_html(page_source[[1]])

job_names  <- html_source %>% html_nodes('.jobname_1 a') %>% html_text()
comp_names <- html_source %>% html_nodes('.compname_1 a') %>% html_text()
job_urls  <- html_source %>% html_nodes('.jobname_1 a') %>% html_attr('href')

result <- data.frame(name = job_names, comp = comp_names, url = job_urls, stringsAsFactors = FALSE)
View(result)

title_string <- c('[^a-zA-Z]r[^a-zA-Z]', '^r[^a-zA-Z]', '[^a-zA-Z]r$', 'python', '大數據', 'machine', 'learning', '機器學習', 'data', '資料', '數據', '分析', 'analys', '科學', '[^a-zA-Z]ai[^a-zA-Z]', '^ai[^a-zA-Z]', '[^a-zA-Z]ai$', '機器人', '人工智慧')
title_string <- paste0(title_string, collapse = '|')
result <- result[grepl(title_string, result$name),]

filter_string <- c('實習', '工讀', '資料庫', '系統分析')
filter_string <- paste0(filter_string, collapse = '|')
result <- result[!grepl(filter_string, result$name),]

result$description <- ''
result$location    <- ''
result$education   <- ''
result$experience  <- ''
result$tools       <- ''
result$skills      <- ''
result$others      <- ''


for(i in 1:nrow(result)){
  Sys.sleep(runif(1, 0.5, 1.5))
  
  remDr$navigate(result$url[i])
  page_source <- remDr$getPageSource()
  html_source <- read_html(page_source[[1]])
  
  tmp <- html_source %>% html_nodes('.info:nth-child(1) p') %>% html_text()
  result$description[i] <- ifelse(length(tmp)>0, tmp, '')
  result$location[i]    <- html_source %>% html_nodes('.addr') %>% html_text() %>% gsub('地圖找工作', '', .) %>% trimws()
  
  ele_inx <- html_source %>% html_nodes('.info:nth-child(2) dt') %>% html_text()
  ele     <- html_source %>% html_nodes('.info:nth-child(2) dd') %>% html_text()
  
  result$education[i]    <- ele[ele_inx=='學歷要求：']
  result$experience[i]   <- ele[ele_inx=='工作經歷：']
  result$tools[i]        <- ele[ele_inx=='擅長工具：']
  result$skills[i]       <- ele[ele_inx=='工作技能：']
  result$others[i]       <- ele[ele_inx=='其他條件：']
  
  cat("\r", format(round(i/nrow(result)*100, 3), nsmall=3) , ' %')
  Sys.sleep(runif(1, 2, 5))
}
dir.create('output', showWarnings = FALSE)
write.csv(result, paste0('output/', format(Sys.Date(), '%Y%m%d'), '_jobs_result.csv'), row.names = FALSE)
