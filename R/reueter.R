library(XML)
library(digest)
library(tibble)
library(httr)
library(rvest)
library(stringr)
library(dplyr)
library(tidyverse)


#####
#링크 
link = '//*[@id="content"]/section[2]/div/div[1]/section/section/div'
url = "https://www.reuters.com/news/archive/businessnews?view=page&page=1&pageSize=10" %>% 
  read_html() %>% 
  html_nodes(xpath = link) %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  unique()  # 중복된 링크 제거 
  
  
ru = paste0('https://www.reuters.com',url[2]) %>% read_html() %>% html_nodes(".StandardArticleBody_body p") %>% html_text(trim = TRUE)


## 각 링크 찾아가기 
news = tibble()
for(n in 1:10){
  exlink = paste0('https://www.reuters.com',url[n]) %>% read_html() 
  
  news.title = exlink %>% html_nodes(".ArticleHeader_headline") %>% html_text(trim = TRUE)
  news.body = exlink %>% html_nodes(".StandardArticleBody_body p") %>% html_text(trim = TRUE) %>% paste0(collapse = "")
  news.date = exlink %>% html_nodes(".ArticleHeader_date") %>% html_text(trim = TRUE) %>% str_trim() %>% str_split_fixed("/",3)
  
  new = tibble(news.date[1] , news.title,news.body)
  news = bind_rows(news,new)
  }
#####


#for 

SC.reuter = function(topic,pages){

  
  news = tibble()
  for(i in 1:pages){
    
    link = '//*[@id="content"]/section[2]/div/div[1]/section/section/div'
    url = paste0("https://www.reuters.com/news/archive/",topic,"?view=page&page=",i,"&pageSize=10") %>% 
      read_html() %>% 
      html_nodes(xpath = link) %>%
      html_nodes("a") %>%
      html_attr("href") %>%
      unique()  # 중복된 링크 제거 
  
  
    for(n in 1:10){
      exlink = paste0('https://www.reuters.com',url[n]) %>% read_html() 
    
      news.title = exlink %>% html_nodes(".ArticleHeader_headline") %>% html_text(trim = TRUE)
      news.body = exlink %>% html_nodes(".StandardArticleBody_body p") %>% html_text(trim = TRUE) %>% paste0(collapse = "")
      news.date = exlink %>% html_nodes(".ArticleHeader_date") %>% html_text(trim = TRUE) %>% str_trim() %>% str_split_fixed("/",3)
      
      new = tibble("source" = "Reueter","Catergory" = topic, "date" = news.date[1],"time" = news.date[2] , news.title,news.body)
      news = bind_rows(news,new)
    }
    print(c(i,"/",pages))
  }
  return(news)
}




# 목록에서 선택형
SC.reuter.t = function(){
  reuter = c('businessNews','companyNews','wealth','topNews','domesticNews','worldNews')
  x = as.numeric(readline("--카테고리를 선택하라 -- \n 1 : businessNews \n 2 : companyNews \n 3 : wealth \n 4 : topNews \n 5 : domesticNews \n 6 : worldNews \n :"))
  pages = as.numeric(readline("가져올 페이지 수는 ? \n :"))
  topic = reuter[x]
  
  news = tibble()
  for(i in 1:pages){
    
    link = '//*[@id="content"]/section[2]/div/div[1]/section/section/div'
    url = paste0("https://www.reuters.com/news/archive/",topic,"?view=page&page=",i,"&pageSize=10") %>% 
      read_html() %>% 
      html_nodes(xpath = link) %>%
      html_nodes("a") %>%
      html_attr("href") %>%
      unique()  # 중복된 링크 제거 
    
    
    for(n in 1:10){
      exlink = paste0('https://www.reuters.com',url[n]) %>% read_html() 
      
      news.title = exlink %>% html_nodes(".ArticleHeader_headline") %>% html_text(trim = TRUE)
      news.body = exlink %>% html_nodes(".StandardArticleBody_body p") %>% html_text(trim = TRUE) %>% paste0(collapse = "")
      news.date = exlink %>% html_nodes(".ArticleHeader_date") %>% html_text(trim = TRUE) %>% str_trim() %>% str_split_fixed("/",3)
      
      new = tibble("source" = "Reueter","Catergory" = topic, "date" = news.date[1],"time" = news.date[2] , news.title,news.body)
      news = bind_rows(news,new)
    }
    print(c(i,"/",pages))
  }
  return(news)
}

setwd(choose.dir())

## 
reuter = c('businessNews','companyNews','wealth','topNews','domesticNews','worldNews')
SC.reuter.save = function(topic,pages){
  df = SC.reuter(topic,pages)
  name = paste0("Reueter",Sys.time(),topic,pages,"csv")
  write.csv(df,file = name)
  print("완료!")
}

SC.reuter.save('businessNews',5000)
