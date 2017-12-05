# edata
**API wrapper for E-Data (spending.gov.ua). Uses API version 2.0. 

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
