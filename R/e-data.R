org_col_type <- readr::cols (regDate = readr::col_date(format = ""),
                      orgCode = readr::col_character(),
                      orgName = readr::col_character(),
                      kopfg = readr::col_character(),
                      koatu = readr::col_character(),
                      regionName = readr::col_character(),
                      district = readr::col_character(),
                      city = readr::col_character(),
                      districtCity = readr::col_character(),
                      cntRep = readr::col_integer(),
                      cntDoc = readr::col_integer(),
                      cntAddDoc = readr::col_integer(),
                      cntAct = readr::col_integer(),
                      cntPeny = readr::col_integer(),
                      cntSignRep = readr::col_integer(),
                      cntSignCont = readr::col_integer(),
                      cntSignAddCont = readr::col_integer(),
                      cntSignAct = readr::col_integer(),
                      cntSignPeny = readr::col_integer())
temp_dir <- tempdir()
temp_file <- paste0(tempfile(tmpdir = temp_dir), '.csv')


#' Download Ogranizations.
#'
#' Function to download information on all registered organizations on website.
#' @param filename Full path to file where you want organisations to be saved.
#' @keywords download_organisations
#' @export
#' @examples 
#' download_organisations()
download_organisations <- function(filename) {
  url <- "http://api.spending.gov.ua/api/v2/stat/organizations/csv"
  download.file(url, "org_file.zip")
  file_path <- unzip("org_file.zip")
  organisations <- readr::read_delim(file_path, skip = 1, 
                          locale = readr::locale(encoding = "Windows-1251"), delim = ";",
                          col_types = org_col_type)
  file.remove("org_file.zip")
  if (!dir.exists(temp_dir)) {
    dir.create(temp_dir)
  }
  write.csv(organisations, filename, row.names = FALSE)
}

transactions_format <- function(df) {
  if (!is.null(df)) {
    df$doc_date <- as.Date(df$doc_date)
    df$doc_v_date <- as.Date(df$doc_v_date)
    df$trans_date <- as.Date(df$trans_date)
    df$amount <- as.numeric(df$amount)
    df
  }
}

request2df <- function(url, q = list()) {
  q_request <- httr::GET(url = url, query = q)
  q_content <- httr::content(q_request)
  if (length(q_content) > 0) {
    q_content <- lapply(q_content, nulls_to_nas)
    df <- data.frame(matrix(unlist(q_content), byrow = T, ncol = length(q_content[[1]])), stringsAsFactors = F)
    names(df) <- names(q_content[[1]])
    df
  } else {
    NULL
  }
}

#' Get organizations ids
#'
#' Function to get organizations codes ("edrpous") given their names (or the beginnings of their names)
#' @param organizations Character vector of organizations names or the beginnings of their names.  
#' @param fixed Logical. If TRUE, pattern is a string to be matched as is. If FALSE, regular expression is used.
#' @param ignore.case if FALSE, the pattern matching is case sensitive and if TRUE, case is ignored during matching. 
#' @keywords organizations, orgs
#' @export
#' @examples 
#' orgs()
orgs <- function(organizations, fixed = TRUE, ignore.case = TRUE) {
  if (!file.exists(temp_file)) {
    download_organisations(temp_file)
  }
  orgs <- readr::read_csv(temp_file, col_types = org_col_type)
  org_ids <- character()
  org_names <- character()
  for (org in organizations) {
    if (fixed) {
      orgs$selected <- stringr::str_detect(orgs$orgName, stringr::fixed(org, ignore_case = ignore.case))
    } else {
      orgs$selected <- stringr::str_detect(orgs$orgName, stringr::coll(org, ignore_case = ignore.case, locale = "ukr"))
    }
    orgs_ <- orgs[orgs$selected,]
    new_ids <- orgs_$orgCode
    org_names <- c(org_names, orgs_$orgName)
    org_ids <- c(org_ids, new_ids)
  }
  org_ids <- unique(org_ids)
  org_names <- unique(org_names)
  names(org_ids) <- org_names
  org_ids
}

nulls_to_nas <- function(l) {
  lapply(l, function(x) {ifelse(is.null(x), NA, x)})
}

add_mult_parameters <- function(params, param_name) {
  s <- ""
  for (p in params) {
    s <- paste0(s, paste0(param_name, "=", as.character(p), "&"))
  }
  s
}

#' Get regions ids
#'
#' Function to get regions' ids given their names (or the beginnings of their names)
#' @param regions Character vector of regions names or the beginnings of their names. If not specified, the function returns named vector with all regions' ids.
#' @param fixed Logical. If TRUE, pattern is a string to be matched as is. If FALSE, regular expression is used.
#' @param ignore.case if FALSE, the pattern matching is case sensitive and if TRUE, case is ignored during matching. 
#' @keywords regions, regs
#' @export
#' @examples 
#' regs
regs <- function(regions = NULL, fixed = TRUE, ignore.case = TRUE) {
  regions_url <- "http://api.spending.gov.ua/api/v2/regions"
  regions_df <- request2df(regions_url)  
  reg_ids <- character()
  reg_names <- character()
  if (length(regions) > 0) {
    for (r in regions) {
      if (fixed) {
        regions_df$selected <- stringr::str_detect(regions_df$regionName, stringr::fixed(r, ignore_case = ignore.case))
      } else {
        regions_df$selected <- stringr::str_detect(regions_df$regionName, stringr::coll(r, ignore_case = ignore.case, locale = "ukr"))
      }
      
      regions_df_ <- regions_df[regions_df$selected,]
      new_ids <- regions_df_$regionCode
      reg_names <- c(reg_names, regions_df_$regionName)
      reg_ids <- c(reg_ids, new_ids)
    }
  } else {
    reg_ids <- regions_df$regionCode
    reg_names <- regions_df$regionName
  }
  reg_ids <- unique(reg_ids)
  reg_names <- unique(reg_names)
  names(reg_ids) <- reg_names
  reg_ids
}

#' Get 100 biggest transactions
#'
#' Function to load 100 biggest transactions within regions or anywhere.
#' @param regions Integer vector of regions ids. If not present, the function will return 100 biggest transactions in every region and on the national level.
#' @keywords top100
#' @export
#' @examples 
#' top100()
top100 <- function(regions = NULL) {
  url <- "http://api.spending.gov.ua/api/v2/api/transactions/top100?"
  url <- paste0(url, add_mult_parameters(regions, "region"))
  df <- request2df(url)
  transactions_format(df)
}

#' Get transactions 
#'
#' Function to load all transactions, limited by payers' and / or receivers' codes or within sinlge day
#' @param payers_edrpous Character vector of payers' codes ("edrpous") in looked transactions.  
#' @param recievers_edrpous Character vector of receivers' codes ("edrpous") in looked transactions
#' @param regions Integer vector of regions ids. If not present, the function will return transactions in every region and on the national level.
#' @param startdate The first date of wanted period. Format - "yyyy-mm-dd"
#' @param enddate The last date of wanted period. Format - "yyyy-mm-dd"
#' @keywords transactions
#' @export
#' @examples 
#' transactions()
transactions <- function(payers_edrpous = NULL, recievers_edrpous = NULL,
                         regions = NULL,  startdate = NULL, enddate = NULL)
{
  if (is.null(recievers_edrpous) & is.null(payers_edrpous)) {
    if (is.null(startdate) | is.null(enddate)) {
      if (is.null(startdate) & is.null(enddate)) {
        cat("Loading transactions for the last date available...")
        startdate <- last_date()
        enddate <- startdate
      } else {
        if (is.null(startdate)) {
          cat(paste0("Loading transactions for ", enddate))
          startdate <- enddate
        } else {
          cat(paste0("Loading transactions for ", startdate))
          enddate <- startdate
        }
      }
    } else {
      if ( startdate != enddate) {
        stop("Either request transactions for 1 day or \"payers_edrpous\" or \"recipt_edrpous\" or both must be present in request.")
      }
    }
  } 
  url <- "http://api.spending.gov.ua/api/v2/api/transactions/?"
  url <- paste0(url, add_mult_parameters(payers_edrpous, "payers_edrpous"))
  url <- paste0(url, add_mult_parameters(recievers_edrpous, "recipt_edrpous"))
  url <- paste0(url, add_mult_parameters(regions, "regions"))
  df <- request2df(url, q = list(startdate = startdate, enddate = enddate))
  transactions_format(df)
}

#' Get the date of the latest transaction
#'
#' Function to load the date of the latest transaction in spending.gov.ua database
#' @keywords last_date
#' @export
#' @examples 
#' last_date()
last_date <- function() {
  url <- "http://api.spending.gov.ua/api/v2/api/transactions/lastload"
  df <- request2df(url)
  df[,1]
}

