require("pdftools"); require("tm");require("stringr");require("IBrokers")
require("reticulate");require("httr");require("rvest")
# https://www.investing.com/economic-calendar
# https://oui.doleta.gov/press/2021/040821.pdf
# ***************************************************************************
# STORE API KEYS: 
PASS <- new.env()
assign("TDAPIKEY","***********************",envir = PASS)
source_python("~/Desktop/R/authTD.py")
token = readRDS("~/Desktop/R/token90.rds")
# function to open position / send OPEN order to IB
openPOS = function(ticker, shares, action){
# PLACE ORDER FUNCTION
require("IBrokers")
tws = twsConnect(port=7497)
ac <- reqAccountUpdates(tws)
# *********************************************************************
security     = twsSTK(paste(ticker),exch="SMART",primary="ARCA",currency="USD")
securityShrs = as.character(shares)
ACT          = as.character(action) # "BUY" / "SELL"
orderId      = as.numeric(reqIds(tws))
# get quote from TD
bidAsk = getQuoteTD(STKS = ticker)
midPoint = round((bidAsk$bidPrice + bidAsk$askPrice)/2,2)
lmtPRC       = as.numeric(midPoint)
#lmtPRC       = round(ifelse(ACT=="BUY",lmtPRC+0.05,lmtPRC-0.05),2)
myorder      = twsOrder(orderId, orderType = "LMT",lmtPrice = lmtPRC ,outsideRTH = "1",
                        action=ACT, totalQuantity = securityShrs, transmit=FALSE)
placeOrder(tws,security, myorder)
}
# to make requests.. because it is deprecated in the new release of rvest!
request_GET <- function(x, url, ...) {
  x$response <- httr::GET(url, x$config, ..., handle = x$handle)
  x$html <- new.env(parent = emptyenv(), hash = FALSE)
  x$url <- x$response$url
  
  httr::warn_for_status(x$response)
  
  x
}
### SLEEP UNTIL MARKET OPENS 
SLEEEP = function(RELEASE_DATETIME){
  ttt <- RELEASE_DATETIME - Sys.time()
  HMS <- attr(ttt,"units")
  tt <- as.numeric(ttt)
  if(HMS == "hours")
  {
    print(paste0("Will now sleep for: ",tt , " hours"));cat("\n")
    print(paste0("STARTING AT: ",RELEASE_DATETIME));cat("\n")
    Sys.sleep(tt*60*60)
  }
  if(HMS == "mins")
  {
    print(paste0("Will now sleep for: ",tt , " minutes"));cat("\n")
    print(paste0("STARTING AT: ",RELEASE_DATETIME));cat("\n")
    Sys.sleep(tt*60)
  }
  if(HMS == "secs")
  {
    print(paste0("Will now sleep for: ",tt , " seconds"));cat("\n")
    print(paste0("STARTING AT: ",RELEASE_DATETIME));cat("\n")
    Sys.sleep(tt)
  }  
}
# ***************************************************************************
# callback URL for TD API - will most likely make it pull from IB...
callback = "https://127.0.0.1" # from TD App
# get Quote from TD...
getQuoteTD = function(STKS)
{
  api_key = PASS$TDAPIKEY
  btoken = paste0("Bearer ",token$access_token)
  url = paste0("https://api.tdameritrade.com/v1/marketdata/quotes?apikey=",
               api_key,"&symbol=",paste(STKS,collapse = ","))
  pg = html_session(url)
  # get data by passing in url and cookies
  pg <- pg %>% request_GET(paste0("https://api.tdameritrade.com/v1/marketdata/quotes?apikey=",
                                  api_key,"&symbol=",paste(STKS,collapse = ",")),
                           config = httr::add_headers(`Authorization` = btoken))
  
  if(pg$response$status_code == 401)
  {
    token = readRDS("~/Desktop/R/token90.rds")
    token = access_token(refresh_token=token$refresh_token, client_id=api_key)
    
    btoken = paste0("Bearer ",token$access_token)
    url = paste0("https://api.tdameritrade.com/v1/marketdata/quotes?apikey=",
                 api_key,"&symbol=",paste(STKS,collapse = ","))
    pg = html_session(url)
    # get data by passing in url and cookies
    pg <- pg %>% request_GET(paste0("https://api.tdameritrade.com/v1/marketdata/quotes?apikey=",
                                    api_key,"&symbol=",paste(STKS,collapse = ",")),
                             config = httr::add_headers(`Authorization` = btoken))
  }
  # raw data
  data_raw <- httr::content(pg$response)
  tmp = rbindlist(data_raw,fill = TRUE)
  
  qt = tmp[,c("bidPrice","askPrice","lastPrice")]
  qt
}
# ***************************************************************************
# "forecast" initial weekly claims
est = 700000
# get the very next release (every Thursday @ 8:30 AM EST)
thisWK <- seq.Date(Sys.Date(), Sys.Date()+7, "1 day")
dte    <- thisWK[base::which(weekdays(thisWK) == "Thursday")]
# change the release date to match naming convention on Bureau of Labor (BOL)
dte    <- format(dte[1], format="%m%d%y")
file <- paste0(dte,".pdf")
# Timestamp of when Algo should start... ideally on time!
RELEASE_DATETIME = as.POSIXct("2021-04-15 05:30:00")
# ***********************************************
#     ALGO INSTRUCTIONS: 'weeklyClaimsAlgo'
# ***********************************************
weeklyClaimsAlgo = function()
{
  # check to see if the new PDF posted
  url = "https://oui.doleta.gov/press/2021/"
  web = read_html(url)
  web <- web %>% html_text() %>% str_split(pattern= "\n")
  
  # if the PDF is posted on the site then run the following
  if(suppressWarnings(str_detect(string = web, pattern = file)))
  {
    # file = "020421.pdf"
    # file = "031121.pdf"
    # file = "040821.pdf"
    # store time to see how long the process takes
    IN = Sys.time()
    # read in the PDF
    tmp <- pdftools::pdf_text(pdf = paste0("https://oui.doleta.gov/press/2021/",file))
    # break the PDF with new-line delimiter... easier to work with in chunks
    tmp = tmp[1] %>% str_split(pattern="\n", n=25)
    # locate where in the PDF reads "In the week ending..." all releases have been the same
    loc = tmp[[1]] %>% str_detect(pattern = "In the week ending ") %>% base::which()
    # subset location (its in the 1st paragraph somewhere)
    initClaims = tmp[[1]][loc]
    # replace commas with empty space in orde to convert to numeric values
    initClaims<- gsub("\\,","",initClaims)
    # split the vector of words after "adjusted initial claims was"
    initClaims <- str_split(initClaims,pattern = "adjusted initial claims was")
    # get the second item in the list (this is where the number is stored)
    tmp <- initClaims[[1]][2]
    # don't care about the words just numbers... pull all instances of numerical values
    tmp <- regmatches(tmp, gregexpr("[[:digit:]]+", tmp))
    # convert to numeric for comparison
    tmp <- tmp %>% unlist %>% as.numeric
    # initial weekly claims number
    tmp <- tmp[1]
    # System out time
    OUT = Sys.time()
    # usually takes between 2-3 seconds to run the above
    OUT-IN
    # Prints current time
    cat("\nTime: ",paste(Sys.time()))
    # if the number is above estimates then bearish... 
    if(tmp > est){cat("\nBearish: ", tmp, " VS ",est,"\n"); ACT = "SELL"}
    # if the number is below estimates then bullish.. 
    if(tmp < est){cat("\nBullish: ", tmp, " VS ",est,"\n"); ACT = "BUY"}
    # send order to IB.. trading 1 share 
    openPOS(ticker = "SPY", shares = 1, action = ACT)
    return(cat("\nORDER SENT!\n"))
  }else{
    # if the release is not available yet then the system will sleep for 5 seconds and..
    # it will then call the `weeklyClaimsAlgo` function again essentially loop until we get 
    # data
    cat("\nNo DATA YET!\n")
    Sys.sleep(5)
    weeklyClaimsAlgo()
  }
}
# ***************************************************************************
#                           Initial Weekly Claims Algo
# ***************************************************************************
# Ran the night before the release.. will sleep until the Release time
SLEEEP(RELEASE_DATETIME);weeklyClaimsAlgo()

