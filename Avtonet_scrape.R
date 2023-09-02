library(rvest)
library(tidyverse)
library(RSelenium)

link1 <- "https://www.avto.net/Ads/results.asp?znamka=&model=&modelID=&tip=&znamka2=&model2=&tip2=&znamka3=&model3=&tip3=&cenaMin=0&cenaMax=999999&letnikMin=0&letnikMax=2090&bencin=0&starost2=999&oblika=&ccmMin=0&ccmMax=99999&mocMin=&mocMax=&kmMin=0&kmMax=9999999&kwMin=0&kwMax=999&motortakt=&motorvalji=&lokacija=0&sirina=&dolzina=&dolzinaMIN=&dolzinaMAX=&nosilnostMIN=&nosilnostMAX=&lezisc=&presek=&premer=&col=&vijakov=&EToznaka=&vozilo=&airbag=&barva=&barvaint=&EQ1=1000000000&EQ2=1000000000&EQ3=1000000000&EQ4=100000000&EQ5=1000000000&EQ6=1000000000&EQ7=1110100020&EQ8=101000000&EQ9=1000000000&KAT=1010000000&PIA=&PIAzero=&PIAOut=&PSLO=&akcija=&paketgarancije=0&broker=&prikazkategorije=&kategorija=&ONLvid=&ONLnak=&zaloga=10&arhiv=&presort=&tipsort=&stran="

link2 <- "https://www.avto.net/Ads/results.asp?znamka=&model=&modelID=&tip=&znamka2=&model2=&tip2=&znamka3=&model3=&tip3=&cenaMin=0&cenaMax=999999&letnikMin=0&letnikMax=2090&bencin=0&starost2=999&oblika=0&ccmMin=0&ccmMax=99999&mocMin=0&mocMax=999999&kmMin=0&kmMax=9999999&kwMin=0&kwMax=999&motortakt=0&motorvalji=0&lokacija=0&sirina=0&dolzina=&dolzinaMIN=0&dolzinaMAX=100&nosilnostMIN=0&nosilnostMAX=999999&lezisc=&presek=0&premer=0&col=0&vijakov=0&EToznaka=0&vozilo=&airbag=&barva=&barvaint=&EQ1=1000000000&EQ2=1000000000&EQ3=1000000000&EQ4=100000000&EQ5=1000000000&EQ6=1000000000&EQ7=1110100020&EQ8=101000000&EQ9=1000000000&KAT=1010000000&PIA=&PIAzero=&PIAOut=&PSLO=&akcija=0&paketgarancije=&broker=0&prikazkategorije=0&kategorija=0&ONLvid=0&ONLnak=0&zaloga=10&arhiv=0&presort=3&tipsort=DESC&stran=2"

https://www.avto.net/Ads/results.asp?znamka=Abarth&model=&modelID=&tip=&znamka2=&model2=&tip2=&znamka3=&model3=&tip3=&cenaMin=0&cenaMax=999999&letnikMin=0&letnikMax=2090&bencin=0&starost2=999&oblika=0&ccmMin=0&ccmMax=99999&mocMin=0&mocMax=999999&kmMin=0&kmMax=9999999&kwMin=0&kwMax=999&motortakt=0&motorvalji=0&lokacija=0&sirina=0&dolzina=&dolzinaMIN=0&dolzinaMAX=100&nosilnostMIN=0&nosilnostMAX=999999&lezisc=&presek=0&premer=0&col=0&vijakov=0&EToznaka=0&vozilo=&airbag=&barva=&barvaint=&EQ1=1000000000&EQ2=1000000000&EQ3=1000000000&EQ4=1000000000&EQ5=1000000000&EQ6=1000000000&EQ7=1110100120&EQ8=100000000&EQ9=1000000000&KAT=1010000000&PIA=&PIAzero=&PIAOut=&PSLO=&akcija=0&paketgarancije=&broker=0&prikazkategorije=0&kategorija=0&ONLvid=0&ONLnak=0&zaloga=10&arhiv=0&presort=&tipsort=&stran=

https://www.avto.net/Ads/results.asp?znamka=Audi&model=&modelID=&tip=&znamka2=&model2=&tip2=&znamka3=&model3=&tip3=&cenaMin=0&cenaMax=999999&letnikMin=0&letnikMax=2090&bencin=0&starost2=999&oblika=0&ccmMin=0&ccmMax=99999&mocMin=0&mocMax=999999&kmMin=0&kmMax=9999999&kwMin=0&kwMax=999&motortakt=0&motorvalji=0&lokacija=0&sirina=0&dolzina=&dolzinaMIN=0&dolzinaMAX=100&nosilnostMIN=0&nosilnostMAX=999999&lezisc=&presek=0&premer=0&col=0&vijakov=0&EToznaka=0&vozilo=&airbag=&barva=&barvaint=&EQ1=1000000000&EQ2=1000000000&EQ3=1000000000&EQ4=1000000000&EQ5=1000000000&EQ6=1000000000&EQ7=1110100120&EQ8=100000000&EQ9=1000000000&KAT=1010000000&PIA=&PIAzero=&PIAOut=&PSLO=&akcija=0&paketgarancije=&broker=0&prikazkategorije=0&kategorija=0&ONLvid=0&ONLnak=0&zaloga=10&arhiv=0&presort=&tipsort=&stran=
  
https://www.avto.net/Ads/results.asp?znamka=Audi&model=&modelID=&tip=&znamka2=&model2=&tip2=&znamka3=&model3=&tip3=&cenaMin=0&cenaMax=999999&letnikMin=0&letnikMax=2090&bencin=0&starost2=999&oblika=0&ccmMin=0&ccmMax=99999&mocMin=0&mocMax=999999&kmMin=0&kmMax=9999999&kwMin=0&kwMax=999&motortakt=0&motorvalji=0&lokacija=0&sirina=0&dolzina=&dolzinaMIN=0&dolzinaMAX=100&nosilnostMIN=0&nosilnostMAX=999999&lezisc=&presek=0&premer=0&col=0&vijakov=0&EToznaka=0&vozilo=&airbag=&barva=&barvaint=&EQ1=1000000000&EQ2=1000000000&EQ3=1000000000&EQ4=1000000000&EQ5=1000000000&EQ6=1000000000&EQ7=1110100120&EQ8=100000000&EQ9=1000000000&KAT=1010000000&PIA=&PIAzero=&PIAOut=&PSLO=&akcija=0&paketgarancije=&broker=0&prikazkategorije=0&kategorija=0&ONLvid=0&ONLnak=0&zaloga=10&arhiv=0&presort=3&tipsort=DESC&stran=1&SUBmodelsearch=A4&SUBmodgrp1=0

https://www.avto.net/Ads/results.asp?znamka=Audi&model=&modelID=&tip=&znamka2=&model2=&tip2=&znamka3=&model3=&tip3=&cenaMin=0&cenaMax=999999&letnikMin=0&letnikMax=2090&bencin=0&starost2=999&oblika=0&ccmMin=0&ccmMax=99999&mocMin=0&mocMax=999999&kmMin=0&kmMax=9999999&kwMin=0&kwMax=999&motortakt=0&motorvalji=0&lokacija=0&sirina=0&dolzina=&dolzinaMIN=0&dolzinaMAX=100&nosilnostMIN=0&nosilnostMAX=999999&lezisc=&presek=0&premer=0&col=0&vijakov=0&EToznaka=0&vozilo=&airbag=&barva=&barvaint=&EQ1=1000000000&EQ2=1000000000&EQ3=1000000000&EQ4=1000000000&EQ5=1000000000&EQ6=1000000000&EQ7=1110100120&EQ8=100000000&EQ9=1000000000&KAT=1010000000&PIA=&PIAzero=&PIAOut=&PSLO=&akcija=0&paketgarancije=&broker=0&prikazkategorije=0&kategorija=0&ONLvid=0&ONLnak=0&zaloga=10&arhiv=0&presort=3&tipsort=DESC&stran=1&SUBmodelsearch=Q5&SUBmodgrp1=0


https://www.avto.net/Ads/results.asp?znamka=Renault&model=&modelID=&tip=&znamka2=&model2=&tip2=&znamka3=&model3=&tip3=&cenaMin=0&cenaMax=999999&letnikMin=0&letnikMax=2090&bencin=0&starost2=999&oblika=0&ccmMin=0&ccmMax=99999&mocMin=0&mocMax=999999&kmMin=0&kmMax=9999999&kwMin=0&kwMax=999&motortakt=0&motorvalji=0&lokacija=0&sirina=0&dolzina=&dolzinaMIN=0&dolzinaMAX=100&nosilnostMIN=0&nosilnostMAX=999999&lezisc=&presek=0&premer=0&col=0&vijakov=0&EToznaka=0&vozilo=&airbag=&barva=&barvaint=&EQ1=1000000000&EQ2=1000000000&EQ3=1000000000&EQ4=1000000000&EQ5=1000000000&EQ6=1000000000&EQ7=1110100120&EQ8=100000000&EQ9=1000000000&KAT=1010000000&PIA=&PIAzero=&PIAOut=&PSLO=&akcija=0&paketgarancije=&broker=0&prikazkategorije=0&kategorija=0&ONLvid=0&ONLnak=0&zaloga=10&arhiv=0&presort=3&tipsort=DESC&stran=1&SUBmodelsearch=Clio&SUBmodgrp1=0
"https://www.avto.net/Ads/results.asp?znamka=Renault&model=Clio&modelID=&tip=&znamka2=&model2=&tip2=&znamka3=&model3=&tip3=&cenaMin=0&cenaMax=999999&letnikMin=0&letnikMax=2090&bencin=0&starost2=999&oblika=0&ccmMin=0&ccmMax=99999&mocMin=0&mocMax=999999&kmMin=0&kmMax=9999999&kwMin=0&kwMax=999&motortakt=0&motorvalji=0&lokacija=0&sirina=0&dolzina=&dolzinaMIN=0&dolzinaMAX=100&nosilnostMIN=0&nosilnostMAX=999999&lezisc=&presek=0&premer=0&col=0&vijakov=0&EToznaka=0&vozilo=&airbag=&barva=&barvaint=&EQ1=1000000000&EQ2=1000000000&EQ3=1000000000&EQ4=1000000000&EQ5=1000000000&EQ6=1000000000&EQ7=1110100120&EQ8=100000000&EQ9=1000000000&KAT=1010000000&PIA=&PIAzero=&PIAOut=&PSLO=&akcija=0&paketgarancije=&broker=0&prikazkategorije=0&kategorija=0&ONLvid=0&ONLnak=0&zaloga=10&arhiv=0&presort=3&tipsort=DESC&stran=1&subcenaMIN=1&subcenaMAX=" # Doloci subcenaMIn = 1 za oglase brez poklici za ceno
  
paste0("https://www.avto.net/Ads/results.asp?znamka=",
names(models_clean[1]),
"&model=",
models_clean[[1]][1],
"Clio&modelID=&tip=&znamka2=&model2=&tip2=&znamka3=&model3=&tip3=&cenaMin=0&cenaMax=999999&letnikMin=0&letnikMax=2090&bencin=0&starost2=999&oblika=0&ccmMin=0&ccmMax=99999&mocMin=0&mocMax=999999&kmMin=0&kmMax=9999999&kwMin=0&kwMax=999&motortakt=0&motorvalji=0&lokacija=0&sirina=0&dolzina=&dolzinaMIN=0&dolzinaMAX=100&nosilnostMIN=0&nosilnostMAX=999999&lezisc=&presek=0&premer=0&col=0&vijakov=0&EToznaka=0&vozilo=&airbag=&barva=&barvaint=&EQ1=1000000000&EQ2=1000000000&EQ3=1000000000&EQ4=1000000000&EQ5=1000000000&EQ6=1000000000&EQ7=1110100120&EQ8=100000000&EQ9=1000000000&KAT=1010000000&PIA=&PIAzero=&PIAOut=&PSLO=&akcija=0&paketgarancije=&broker=0&prikazkategorije=0&kategorija=0&ONLvid=0&ONLnak=0&zaloga=10&arhiv=0&presort=3&tipsort=DESC&stran=1&subcenaMIN=1&subcenaMAX=")


# Setting up the remote server
remDr <- remoteDriver(
  remoteServerAddr = "192.168.0.101",
  port = 4445L
)

remDr$open()

remDr$navigate("https://www.avto.net/Ads/search_makes.asp") #Navigating to the car brands page

page <- remDr$getPageSource()[[1]]

pago <- read_html(page)

brands <- pago %>% html_nodes(".d-block") %>% html_text() %>% str_trim() #Extracting brands

brands <- brands[1:91] #Removing the additional information not related to the car brands
brands <- gsub("\\s*\\(\\d+\\)", "", brands) #Extracting just the brand names
brands <- gsub("[ -]", "", brands) #Removing spaces and dashes
brands <- gsub("Š", "S", brands) #Replacing Š with S in Škoda for later compatibility


models <- list() #Initiating empty list for looping over the brands and models


#Constructing a loop to go over the brands and collect the information about the models
for(i in seq_along(brands)) {
  link <- paste0("https://www.avto.net/Ads/results.asp?znamka=",
                 brands[i],
                 "&model=&modelID=&tip=&znamka2=&model2=&tip2=&znamka3=&model3=&tip3=&cenaMin=0&cenaMax=999999&letnikMin=0&letnikMax=2090&bencin=0&starost2=999&oblika=0&ccmMin=0&ccmMax=99999&mocMin=0&mocMax=999999&kmMin=0&kmMax=9999999&kwMin=0&kwMax=999&motortakt=0&motorvalji=0&lokacija=0&sirina=0&dolzina=&dolzinaMIN=0&dolzinaMAX=100&nosilnostMIN=0&nosilnostMAX=999999&lezisc=&presek=0&premer=0&col=0&vijakov=0&EToznaka=0&vozilo=&airbag=&barva=&barvaint=&EQ1=1000000000&EQ2=1000000000&EQ3=1000000000&EQ4=1000000000&EQ5=1000000000&EQ6=1000000000&EQ7=1110100120&EQ8=100000000&EQ9=1000000000&KAT=1010000000&PIA=&PIAzero=&PIAOut=&PSLO=&akcija=0&paketgarancije=&broker=0&prikazkategorije=0&kategorija=0&ONLvid=0&ONLnak=0&zaloga=10&arhiv=0&presort=&tipsort=&stran=")
  
  remDr$navigate(link)
  
  Sys.sleep(2)
  
  page <- remDr$getPageSource()[[1]]
  
  Sys.sleep(1)
  
  pago <- read_html(page)
  
  
  model_names1 <- pago %>% html_nodes(".border-bottom:nth-child(1) span:nth-child(1)") %>% html_text() %>% str_trim()
  model_names1 <- unique(model_names1)
  model_names2 <- pago %>% html_nodes(".border-bottom:nth-child(2) span:nth-child(1)") %>% html_text() %>% str_trim()
  model_names2 <- unique(model_names2)
  model_names <- c(model_names1, model_names2)
  
  models[[i]] <- model_names
  
}

remDr$close()

names(models) <- brands #Naming the list
saveRDS(models, "avtonet_models.RData")

#Removing some unwanted parts
models_clean <- lapply(models, function(x) str_remove_all(x, "PRODAM|kombilim\\. / hatchback|coupe|kabriolet|kombilim\\. / hatchback|terensko vozilo / SUV|
                                              limuzina|karavan|pick up|limuzina|Video klic / Virtual\\.ogled|Online Nakup|enoprostorec / van"))

indices <- lapply(models_clean, nzchar) #Extracting the indices of empty strings inside the sublists
models_clean <- Map(function(x, y) x[y], models_clean, indices) #Removing the empty strings
models_clean <- lapply(models_clean, function(sublist) sublist[-length(sublist)]) #Removing the brand name from the last part

saveRDS(models_clean, "avtonet_models.RData")
models_clean <- readRDS("avtonet_models.RData")

######## Poskusamo navigirat med stranmi po avtonet
remDr$navigate("https://www.avto.net/Ads/results.asp?znamka=Renault&model=Clio&modelID=&tip=&znamka2=&model2=&tip2=&znamka3=&model3=&tip3=&cenaMin=0&cenaMax=999999&letnikMin=0&letnikMax=2090&bencin=0&starost2=999&oblika=0&ccmMin=0&ccmMax=99999&mocMin=0&mocMax=999999&kmMin=0&kmMax=9999999&kwMin=0&kwMax=999&motortakt=0&motorvalji=0&lokacija=0&sirina=0&dolzina=&dolzinaMIN=0&dolzinaMAX=100&nosilnostMIN=0&nosilnostMAX=999999&lezisc=&presek=0&premer=0&col=0&vijakov=0&EToznaka=0&vozilo=&airbag=&barva=&barvaint=&EQ1=1000000000&EQ2=1000000000&EQ3=1000000000&EQ4=1000000000&EQ5=1000000000&EQ6=1000000000&EQ7=1110100120&EQ8=100000000&EQ9=1000000000&KAT=1010000000&PIA=&PIAzero=&PIAOut=&PSLO=&akcija=0&paketgarancije=&broker=0&prikazkategorije=0&kategorija=0&ONLvid=0&ONLnak=0&zaloga=10&arhiv=0&presort=3&tipsort=DESC&stran=1&subcenaMIN=1&subcenaMAX=")

page <- remDr$getPageSource()[[1]]

pago <- read_html(page)

pago %>% html_nodes("td.w-75.pl-3") %>% html_text() #Datum 1. registracije
pago %>% html_nodes("td.pl-3") %>% html_text() #Vse info iz kartice
pago %>% html_nodes("div.col-auto.p-3.GO-Results-Photo") %>% html_text() %>% str_trim() #Informacije, ce je vozilo v okvari

pago %>% html_nodes("div.d-none.d-sm-block.col-auto.px-sm-0.pb-sm-3") %>% html_text() %>% str_trim() #Cene avtov

pago %>% html_nodes(".GO-Rounded-T") %>% html_text() #Stevilo oglasov

button <- remDr$findElement(using = "css selector", value = "li.page-item.GO-Rounded-R")
button$clickElement()

#Loopanje po oglasih znamke in modelov
car_prices <- list()

remDr$navigate("https://www.avto.net/Ads/results.asp?znamka=Renault&model=Clio&modelID=&tip=&znamka2=&model2=&tip2=&znamka3=&model3=&tip3=&cenaMin=0&cenaMax=999999&letnikMin=0&letnikMax=2090&bencin=0&starost2=999&oblika=0&ccmMin=0&ccmMax=99999&mocMin=0&mocMax=999999&kmMin=0&kmMax=9999999&kwMin=0&kwMax=999&motortakt=0&motorvalji=0&lokacija=0&sirina=0&dolzina=&dolzinaMIN=0&dolzinaMAX=100&nosilnostMIN=0&nosilnostMAX=999999&lezisc=&presek=0&premer=0&col=0&vijakov=0&EToznaka=0&vozilo=&airbag=&barva=&barvaint=&EQ1=1000000000&EQ2=1000000000&EQ3=1000000000&EQ4=1000000000&EQ5=1000000000&EQ6=1000000000&EQ7=1110100120&EQ8=100000000&EQ9=1000000000&KAT=1010000000&PIA=&PIAzero=&PIAOut=&PSLO=&akcija=0&paketgarancije=&broker=0&prikazkategorije=0&kategorija=0&ONLvid=0&ONLnak=0&zaloga=10&arhiv=0&presort=3&tipsort=DESC&stran=1&subcenaMIN=1&subcenaMAX=")
page_source <- remDr$getPageSource()[[1]]

page_num <- 1
while (TRUE) {
  # Check if we have reached the final page
  if (page_num > 20) {
    break
  }
  
  # Click the "next page" button
  
  
  Sys.sleep(3)
  
  page <- remDr$getPageSource()[[1]]
  
  pago <- read_html(page)
  prices <- pago %>% html_nodes("div.d-none.d-sm-block.col-auto.px-sm-0.pb-sm-3") %>% html_text() %>% str_trim()
  
  car_prices[[page_num]] <- prices
  
  button <- remDr$findElement(using = "css selector", value = "li.page-item.GO-Rounded-R")
  button$clickElement()
  
  # Wait for the page to load
  Sys.sleep(3)
  
  # Check if the page source has changed
  if (remDr$getPageSource()[[1]] == page_source) {
    break
  }
  
  # Get the new page source and update the page number
  page_source <- remDr$getPageSource()[[1]]
  page_num <- page_num + 1
}


#Poskusamo linke 
remDr$navigate(link)

link <- paste0("https://www.avto.net/Ads/results.asp?znamka=",
       names(models_clean[5]),
       "&model=",
       models_clean[[5]][2],
       "&modelID=&tip=&znamka2=&model2=&tip2=&znamka3=&model3=&tip3=&cenaMin=0&cenaMax=999999&letnikMin=0&letnikMax=2090&bencin=0&starost2=999&oblika=0&ccmMin=0&ccmMax=99999&mocMin=0&mocMax=999999&kmMin=0&kmMax=9999999&kwMin=0&kwMax=999&motortakt=0&motorvalji=0&lokacija=0&sirina=0&dolzina=&dolzinaMIN=0&dolzinaMAX=100&nosilnostMIN=0&nosilnostMAX=999999&lezisc=&presek=0&premer=0&col=0&vijakov=0&EToznaka=0&vozilo=&airbag=&barva=&barvaint=&EQ1=1000000000&EQ2=1000000000&EQ3=1000000000&EQ4=1000000000&EQ5=1000000000&EQ6=1000000000&EQ7=1110100120&EQ8=100000000&EQ9=1000000000&KAT=1010000000&PIA=&PIAzero=&PIAOut=&PSLO=&akcija=0&paketgarancije=&broker=0&prikazkategorije=0&kategorija=0&ONLvid=0&ONLnak=0&zaloga=10&arhiv=0&presort=3&tipsort=DESC&stran=1&subcenaMIN=1&subcenaMAX=")

pago %>% html_nodes("div.col-auto.p-3.GO-Results-Photo") %>% html_text() %>% str_trim()


remDr$open()
#Poskusamo naredit loop cez Alfa Romeo
car_prices <- list()

remDr$navigate("https://www.avto.net/Ads/search_makes.asp")
page_source <- remDr$getPageSource()[[1]]

page_num <- 1


car_prices <- list()

for(i in seq_along(models_clean[[5]])) {
  
  page_num <- 1
  
  link <- paste0("https://www.avto.net/Ads/results.asp?znamka=",
                 names(models_clean)[5],
                 "&model=",
                 models_clean[[5]][i],
                 "&modelID=&tip=&znamka2=&model2=&tip2=&znamka3=&model3=&tip3=&cenaMin=0&cenaMax=999999&letnikMin=0&letnikMax=2090&bencin=0&starost2=999&oblika=0&ccmMin=0&ccmMax=99999&mocMin=0&mocMax=999999&kmMin=0&kmMax=9999999&kwMin=0&kwMax=999&motortakt=0&motorvalji=0&lokacija=0&sirina=0&dolzina=&dolzinaMIN=0&dolzinaMAX=100&nosilnostMIN=0&nosilnostMAX=999999&lezisc=&presek=0&premer=0&col=0&vijakov=0&EToznaka=0&vozilo=&airbag=&barva=&barvaint=&EQ1=1000000000&EQ2=1000000000&EQ3=1000000000&EQ4=1000000000&EQ5=1000000000&EQ6=1000000000&EQ7=1110100120&EQ8=100000000&EQ9=1000000000&KAT=1010000000&PIA=&PIAzero=&PIAOut=&PSLO=&akcija=0&paketgarancije=&broker=0&prikazkategorije=0&kategorija=0&ONLvid=0&ONLnak=0&zaloga=10&arhiv=0&presort=3&tipsort=DESC&stran=1&subcenaMIN=1&subcenaMAX=")
  
  remDr$navigate(link)
  
  Sys.sleep(3)
  
  while (TRUE) {
    # Check if we have reached the final page
    if (page_num > 20) {
      break
    }
    
    # Click the "next page" button

    page <- remDr$getPageSource()[[1]]
    
    pago <- read_html(page)
    prices <- pago %>% html_nodes("div.d-none.d-sm-block.col-auto.px-sm-0.pb-sm-3") %>% html_text() %>% str_trim()
    
    car_prices[[page_num]] <- prices
    
    
    tryCatch({
      
      button <- remDr$findElement(using = "css selector", value = "li.page-item.GO-Rounded-R")
      
      button$clickElement()
    }, error = function(e) {
      if(inherits(e, "NoSuchElementError")) {
        break
      } else {
        button$clickElement()
      }
    })
    
    
    # Wait for the page to load
    Sys.sleep(3)
    
    # Check if the page source has changed
    if (remDr$getPageSource()[[1]] == page_source) {
      break
    }
    
    # Get the new page source and update the page number
    page_source <- remDr$getPageSource()[[1]]
    page_num <- page_num + 1
  }
  
  
}
