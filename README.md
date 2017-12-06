# edata
**API wrapper for E-Data (spending.gov.ua). Uses API version 2.0. **

To install and load:
>devtools::install_git("https://github.com/texty/edata")

>library(edata)

**The functions of the package are:**


  * regs - extracts regions' ids given its names or regex
  * orgs - extracts organizations' ids given its names or regex
  * last_date - gets the data of last update of transactions database
  * top100 - gets 100 biggest transactions of all time
  * transactions - loads all transactions, limited by payers' and / or receivers' codes or within single day
  * download_organizations - loads information on all registered organizations on website.


**Examples**

>library(tidyverse)

To get all transactions of Kyiv city council:

>kyivrada_transactions <- 

>  orgs("Київська міська рада") %>%

>  transactions()

To get top100 transactions in Kharkiv oblast:

>kharkiv_top100 <- 

>  regs("Харківська") %>%

>  top100()

To download full list of organizations and statistics of their transactions:

>download_organisations(filename = "organisations.csv")
