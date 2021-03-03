library(tidyverse)
library(rvest)
library(rio)
library(here)
library(httr)
library(RSelenium)
library(stringr)
library(rtweet)
source("src/QMsaK_functions.R")

# Download Docker Desktop from https://www.docker.com/get-started for your OS

# To download the docker image for Selenium in Chrome or Firefox.
system("docker run --name chrome_server  --detach --publish 4445:4444 --publish 5901:5900 selenium/standalone-chrome-debug")
system("docker run --name firefox_server  --detach --publish 4445:4444 --publish 5901:5900 selenium/standalone-firefox-debug")

# Open Docker, go to "Images", click "Run" and select "Optional Setting".
# Set the port to 4445 and add a second port with "+" setting it to 5899. Run it.
# Now you have created a new container image of Selenium. 
# This is necessary because the initial ones don't work. 

# Afterwards, you will launch your browser inside this container image. 
# Since it does not have a UI and you will want to
# see what your code is doing step by step you need another program that is able
# to see inside the container image. Go to https://www.realvnc.com/en/connect/download/viewer/macos/, 
# and download and install VNC Viewer for your OS. 
# Launch VNC Viewer and connect to 127.0.0.1:5899, the password is "secret" by default.

# Check out these pages or directly go ahead with the code below.
# http://joshuamccrain.com/tutorials/web_scraping_R_selenium.html
# https://www.rdocumentation.org/packages/seleniumPipes/versions/0.3.7/topics/findElement

# Scraping the names of all twenty constituents of the Swiss Market Index
html <- html("https://www.finanzen.net/index/smi/werte")
name <- html %>% 
  html_nodes(".table-hover a") %>% 
  html_text()

# Remove parentheses and the text within from company names for better performance
# when using Selenium afterwards.
name <- gsub("\\s*\\([^\\)]+\\)","", name)
name[13] <- "SGS"
save(name, file = here::here("data/name.RData"))

# Create a container for the companies' stock market codes
stock_code <- vector(mode = "character", length = length(name))

# Unfortunately the more convenient seleniumPipes has a bug, but it would work like this:
#remDr <- remoteDr(browserName = "chrome", port=4445L)
#remDr %>% 
#go("http://www.google.com/ncr") %>% 
#findElement(using = "name", value = "q")

# Instead we use the RSelenium standard notation.
rD <- rsDriver(browser="firefox", port=4445L, verbose=F)
remDr <- rD[["client"]]

# Before running the loop below, make sure to restart R and close all windows in
# standalone Browser. Then run the script until you reach this text here. Right
# after running the loop, click the "Accept" on the Google form quickly. If you
# are not quick enough, the code will raise an error. Then just run the code again,
# this time without restarting R, etc. Now it should work.
# The thing is, when the data regulations form is raised the first time,
# remDr$getPageSource will download an erroneous page source.
remDr$open()
for (i in 1:length(stock_code)) {
  Sys.sleep(1)
  remDr$navigate("https://www.google.com/")
  Sys.sleep(2)
  remDr$findElement("name", "q")$sendKeysToElement(list(paste(name[i], "stock")))
  Sys.sleep(2)
  remDr$findElements("name", "btnK")[[1]]$clickElement()
  Sys.sleep(3)
  html <- remDr$getPageSource()[[1]]
  Sys.sleep(3)
  stock_code[i] <- read_html(html) %>% 
    html_nodes(css = ".HfMth") %>% 
    html_text()
  stock_code[i] <- strsplit(stock_code[i], " ")[[1]][2]
}
stock_code[1] <- "ABBN"
stock_code[12] <- "RHHBY"
save(stock_code, file = here::here("data/stock_code.RData"))

#Get the companies' ESG scores from MSCI
esg_score <- vector(mode = "character", length = length(name))
remDr$open()
for (i in 1:length(esg_score)) {
  remDr$navigate("https://www.msci.com/our-solutions/esg-investing/esg-ratings/esg-ratings-corporate-search-tool")
  Sys.sleep(2)
  remDr$findElement(using = "class", value = "gdpr-allow-cookies")$clickElement()
  Sys.sleep(2)
  remDr$findElement(using = "css", value = "#_esgratingsprofile_keywords")$sendKeysToElement(list(stock_code[i]))
  Sys.sleep(3)
  remDr$findElement(using = "css", value = "#ui-id-1")$clickElement()
  Sys.sleep(3)
  html1 <- remDr$getPageSource()[[1]]
  esg_score_class <- read_html(html1) %>%
    html_nodes(css = ".ratingdata-company-rating") %>% 
    html_attr("class") %>% 
    esg_score_getter()
  esg_score[i] <- esg_score_class
  Sys.sleep(2)
}
save(esg_score, file = here::here("data/esg_score.RData"))

#Alcon, Richemont and the Swatch Group do not have Twitter accounts. In the case of Alcon
#there is no account to be found. In the case of the two former, they have Twitter accounts
#for individual brands that are part of the group but not for the group itself.
twitter_name <- vector(mode = "character", length = length(name))
remDr$open()
for (i in 1:length(twitter_name)) {
  remDr$navigate("https://duckduckgo.com/")
  Sys.sleep(2)
  remDr$findElement(using = "id", value = "search_form_input_homepage")$sendKeysToElement(list(paste(name[i], "group", "twitter")))
  Sys.sleep(2)
  remDr$findElement(using = "id", value = "search_button_homepage")$clickElement()
  Sys.sleep(3)
  remDr$findElement(using = "css", value = "#r1-0 > div:nth-child(1) > h2:nth-child(1) > a:nth-child(1)")$clickElement()
  Sys.sleep(3)
  html2 <- remDr$getPageSource()[[1]]
  tryCatch(
    {
      twitter <- read_html(html2) %>%
        html_nodes(css = "div.r-15d164r:nth-child(2) > div:nth-child(1) > div:nth-child(1) > div:nth-child(2) > div:nth-child(1) > span:nth-child(1)") %>% 
        html_text()
      twitter_name[i] <- twitter
    }, error = function(cond) {
      twitter_name[i] <- "No twitter account found"
    }
  )
  Sys.sleep(2)
}
twitter_name[2] <- ""
twitter_name[11] <- ""
twitter_name[14] <- "@Sika"
twitter_name[15] <- ""
twitter_name[18] <- ""
save(twitter_name, file = here::here("data/twitter_name.RData"))

companies <- data.frame(name = comp_empl, 
                        stock_code = comp_stock_code,
                        msci_esg_score = comp_esg_scores)

for (i in twitter_name) {
  twitter_feed <- get_timeline(twitter_name[i], n = 1000) %>% 
                    filter(is_retweet == FALSE)
  save(twitter_feed, file = paste0("data/", name[i], "_twitter_feed", ".RData"))
}






