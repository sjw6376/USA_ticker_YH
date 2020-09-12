library(stringr)
library(rvest)
library(readr)
library(httr)
# 먼저 csv파일 불러오는거부터.
#options("scipen" = 100)
usa_ticker = read.csv('data/total.csv', row.names = 1, stringsAsFactors = F)

usa_unit <- str_match(usa_ticker [, 2], '[a-zA-Z]')
usa_multiplier <- list()

for (i in 1: nrow(usa_unit)){
  usa_multiplier[i] <- switch(usa_unit[i], 'M' = {1000000} , 'B' = {1000000000}, 'T' = {1000000000000})
}

usa_ticker[ , 2] <- usa_ticker [ ,2 ] %>%  str_match('[0-9.0-9]*') %>% as.numeric()

unlist(usa_multiplier) -> usa_multiplier #결측값 날리기
usa_ticker <- na.omit(usa_ticker) #왜 결측값이 하나가 안맞지.

usa_ticker[,2] <- usa_ticker[,2] * usa_multiplier

#시가 총액 구한거까지 다 했음. 0값이 있어서 제대로 되지 않았다. 0값 위치는 which로 찾으면 편하다.

write.csv(usa_ticker,'data/usa_ticker.csv')

#X-path를 이용해 GP와 A 끌어오기로 하자. 이거는 아직 못했음.
#GP 위치의 X path = '//*[@id="Col1-1-Financials-Proxy"]/section/div[4]/div[1]/div[1]/div[2]/div[3]/div[1]/div[2]/span'
# //*[@id="Col1-1-Financials-Proxy"]/section/div[4]/div[1]/div[1]/div[2]/div[3]/div[1]/div[2]/span
usa_ticker['GP'] <- 0
headers = c('User-Agent' = 'Mozila/5.0')

for (i in 1: nrow(usa_ticker)){
  ticker = usa_ticker[i,1]
  url = paste0('https://finance.yahoo.com/quote/',ticker,'/financials?p=',ticker)
  gp <- read_html(url) %>% html_node(xpath = '//*[@id="Col1-1-Financials-Proxy"]/section/div[4]/div[1]/div[1]/div[2]/div[3]/div[1]/div[2]/span') %>% html_text()
  usa_ticker[i,3] <- gp
}
