#' @title Gather Data From Scopus
#'
#' @description This function gather data from Scopus database. You just need to input the journal names that you want to do a research with, and api key and token that you got from Scopus, then you can get the data.
#' The data will include these columns:
#' "AU" -- Authors??? Names;
#' "TI" -- Document Title;
#' "AID" -- Author's ID;
#' "PY" -- Publication Year;
#' "SO" -- Journal Name (or Source);
#' "TC" -- Times Cited;
#' "C1" -- Authors??? Affiliations;
#' "AB" -- Abstract;
#' "DE" -- Authors??? Keywords;
#' "ID" -- Keywords associated by SCOPUS or WoS database;
#' "CR" -- Cited References;
#' "RP" -- Corresponding Author???s Affiliation;
#' "JI" -- ISO Source Abbreviation;
#' "DT" -- Document Type;
#' "DB" -- Bibliographic Database;
#' "UT" -- Unique Article Identifier;
#'

#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
#' @examples
#' gather_scopus()
gather_scopus <- function(journals, api_key, insttoken, abbreviate = FALSE, year_greater = NULL, year_less = NULL){


  my_full_list = list()

  for(j in journals){
    my_list = list()
    message(j)
    query = paste0('issn(', paste0(j), ')')
    query = URLencode(query, reserved = T)



    base_url = paste0('https://api.elsevier.com/content/search/scopus?query=',query,'&apiKey=', api_key, '&insttoken=', insttoken, '&view=COMPLETE&pubyear%3E', year_greater,'pubyear%3C', year_less)

    et <- GET(base_url) %>%
      content("text") %>%
      fromJSON()
    og <- as.numeric((GET(base_url) %>%
                        content("text") %>%
                        fromJSON())$`search-results`$`opensearch:totalResults`)%/%25
    og <- ifelse(abbreviate != F, abbreviate, og)
    message(paste("Destination:", og))
    for(i in 0:og){
      message(i)
      url = paste0(base_url, '&start=',1+i*25)
      red <- GET(url)

      md <- red %>% content("text") %>% fromJSON()
      aff <- cbind(md$`search-results`$entry$affiliation %>% map_chr(function(x) paste(x$afid, collapse = "|")) %>% tibble() %>% separate_rows(1, sep = "\\|"),
                   md$`search-results`$entry$affiliation %>% map_chr(function(x) paste(x$affilname, collapse = "|")) %>% tibble() %>% separate_rows(1, sep = "\\|")) %>%
        `colnames<-`(c("afid", "affiliation")) %>%
        as_tibble()
      refs <- NA #c()
      # for(k in md$`search-results`$entry$eid){
      #   #message(k)
      #   earl = paste0('https://api.elsevier.com/content/abstract/eid/',k ,'?apiKey=', api_key, '&insttoken=', insttoken, '&view=REF') %>%
      #     GET() %>% content("text") %>% fromJSON()
      #   # Ok, so my next task is to rebuild the APA citation from this. That is cool.
      #   refs[k] <- if(length(earl$`abstracts-retrieval-response`$references$reference$url) > 1){
      #     paste0(
      #       tryCatch(earl$`abstracts-retrieval-response`$references$reference$`author-list`$author %>% sapply(function(x) paste0(unique(x$`ce:indexed-name`), collapse = ", " )),
      #                error = function(e){replace_na(earl$`abstracts-retrieval-response`$references$reference$`author-list`, "")}),
      #       ", ",
      #       earl$`abstracts-retrieval-response`$references$reference$title %>% replace_na(""),
      #       ", (",
      #       earl$`abstracts-retrieval-response`$references$reference$`prism:coverDate` %>% paste0() %>% as_date() %>% year() %>% replace_na(""),
      #       "), ",
      #       earl$`abstracts-retrieval-response`$references$reference$sourcetitle %>% replace_na(""),
      #       ", ",
      #       earl$`abstracts-retrieval-response`$references$reference$url) %>% paste0(collapse = "; ")
      #   }else{
      #     paste0(earl$`abstracts-retrieval-response`$references$reference$`author-list`$author$`ce:indexed-name` %>%  paste0(unique(.), collapse = ", " ),
      #            ", ",
      #            earl$`abstracts-retrieval-response`$references$reference$title %>% replace_na(""),
      #            ", (",
      #            earl$`abstracts-retrieval-response`$references$reference$`prism:coverDate` %>% paste0() %>% as_date() %>% year() %>% replace_na(""),
      #            "), ",
      #            earl$`abstracts-retrieval-response`$references$reference$sourcetitle %>% replace_na(""),
      #            ", ",
      #            earl$`abstracts-retrieval-response`$references$reference$url) %>% paste0(collapse = "; ")
      #   }
      #   Sys.sleep(1/9)
      #
      # }
      #
      keywords = if(length(str_replace_all(md$`search-results`$entry$authkeywords, " \\| ", ";"))==0){
        NA
      } else{
        str_replace_all(md$`search-results`$entry$authkeywords, " \\| ", ";")
      }

      if(is.null(md$`search-results`$entry$author)&is.null(md$`search-results`$entry$`dc:title`)){
        message(i)
        next()
      }
      nobs <- length(md$`search-results`$entry$eid)
      ifnaclean <- function(x, y, nobs){
        if(length(x)==0){
          rep(NA, nobs)
        }else if(is.na(x)|is.null(x)){
          rep(NA, nobs)
        }else{
          y
        }
      }
      # this is what i am working for.
      #"AU" "TI" "SO" "JI" "DT" "DE" "ID" "AB" "C1" "RP" "CR" "TC" "PY" "UT" "DB"
      my_list[[as.character(i)]] <- tibble(TI = md$`search-results`$entry$`dc:title`,
                                           AU = ifnaclean(x = md$`search-results`$entry$author, y = md$`search-results`$entry$author  %>%
                                                            map_chr(function(x) paste(x$authname, collapse = ";")), nobs = nobs),

                                           AID = ifnaclean(x = md$`search-results`$entry$author, y = md$`search-results`$entry$author  %>%
                                                             map_chr(function(x) paste(x$authid, collapse = ";")), nobs = nobs),
                                           PY = md$`search-results`$entry$`prism:coverDate` %>% lubridate::as_date() %>% lubridate::year(),
                                           SO = md$`search-results`$entry$`prism:publicationName`,
                                           # VL = md$`search-results`$entry$`prism:volume`,
                                           # IS = md$`search-results`$entry$`prism:issueIdentifier`,
                                           # Art..No. = md$`search-results`$entry$eid,
                                           # Page.start = str_split(md$`search-results`$entry$`prism:pageRange`, "-") %>% map_int(function(x) as.integer(x[1])),
                                           # Page.end = str_split(md$`search-results`$entry$`prism:pageRange`, "-") %>% map_int(function(x) as.integer(x[2])),
                                           # PP = str_split(md$`search-results`$entry$`prism:pageRange`, "-") %>% map_int(function(x) as.integer(x[2])-as.integer(x[1])),
                                           TC = md$`search-results`$entry$`citedby-count`,
                                           # DI = if(is.null(md$`search-results`$entry$`prism:doi`)){md$`search-results`$entry$eid}else{md$`search-results`$entry$`prism:doi`},
                                           # URL = md$`search-results`$entry$`prism:url`,
                                           # Affiliations = md$`search-results`$entry$affiliation  %>%
                                           #   map_chr(function(x) paste(x$affilname, collapse = ";")),
                                           C1 = if(length(md$`search-results`$entry$affiliation  %>%
                                                          map_chr(function(x) paste(x$affilname, collapse = ";")))>1){
                                             md$`search-results`$entry$affiliation  %>%
                                               map_chr(function(x) paste(x$affilname, collapse = ";"))
                                           }else{rep(NA, nobs)} , # This should be authors and affiliations
                                           AB = if(is.null(md$`search-results`$entry$`dc:description`)){rep(NA, nobs)}else{md$`search-results`$entry$`dc:description`},
                                           DE = keywords,
                                           ID = keywords,
                                           # Molecular.Sequence.Numbers = NA,
                                           # Chemicals.CAS = NA,
                                           # Tradenames = NA,
                                           # Manufacturers = NA,
                                           # FU = NA,
                                           CR = refs,
                                           RP = NA, # what is rp?
                                           # Editors = NA, Sponsors = NA, Publisher = NA, Conference.name = NA,
                                           # Conference.date = NA, Conference.location = NA, Conference.code = NA,
                                           # ISSN = ifelse(is.null(md$`search-results`$entry$`prism:issn`), md$`search-results`$entry$`prism:eIssn`, md$`search-results`$entry$`prism:issn`),
                                           # ISBN = NA,
                                           # CODEN = NA,
                                           # PubMed.ID = NA,
                                           # LA = "ENGLISH",
                                           JI = md$`search-results`$entry$`prism:publicationName` %>% abbreviate(),
                                           DT = md$`search-results`$entry$subtypeDescription,
                                           # Publication.Stage = NA,
                                           # Access.Type = NA,
                                           DB = "Scopus",
                                           UT = md$`search-results`$entry$eid, # what is Ut?
                                           domestic =if(is.null(md$`search-results`$entry$affiliation)){rep(NA, nobs)}else{md$`search-results`$entry$affiliation %>% map_lgl(function(x) if(!is.null(x)){x %>% pull(`affiliation-country`) %>% str_detect("United States") %>% any()}else{NA})}
                                           # J9 = NA,
                                           # AU_UN = md$`search-results`$entry$affiliation  %>%
                                           #   map_chr(function(x) paste(x$affilname, collapse = ";")),
                                           # AU1_UN = NA,
                                           # AU_UN_NR = NA,
                                           # SR_FULL = NA,
                                           # SR = paste(
                                           #   md$`search-results`$entry$`dc:creator`,
                                           #   md$`search-results`$entry$`prism:coverDate` %>% lubridate::as_date() %>% lubridate::year(),
                                           #   md$`search-results`$entry$`prism:publicationName` %>% abbreviate(),
                                           #   sep = ", ")
      )

      my_full_list[[j]] = my_list %>%
        bind_rows() %>%
        distinct
      write_csv(my_full_list %>%
                  bind_rows() %>%
                  distinct %>%
                  filter(domestic == T) %>%
                  select(AU,TI,SO,JI,DT,DE,ID,AB,C1,RP,CR,TC,PY,UT,DB,AID) %>%
                  as.data.frame(), #%>% column_to_rownames(row),
                paste0("scopus", gsub(" ", "_", Sys.Date()), "_domestic.csv"))


    }







    my_full_list[[j]] = my_list %>%
      bind_rows() %>%
      distinct
    write_csv(my_full_list %>%
                bind_rows() %>%
                distinct %>%
                select(AU,TI,SO,JI,DT,DE,ID,AB,C1,RP,CR,TC,PY,UT,DB,AID) %>%
                as.data.frame(), #%>% column_to_rownames(row),
              paste0("scopus", gsub(" ", "_", Sys.Date()), ".csv"))
  }
  write_csv(my_full_list %>%
              bind_rows() %>%
              distinct %>%
              select(AU,TI,SO,JI,DT,DE,ID,AB,C1,RP,CR,TC,PY,UT,DB) %>%
              as.data.frame(), #%>% column_to_rownames(row),
            paste0("scopus", gsub(" ", "_", Sys.Date()), ".csv"))

  return(my_full_list %>%
           bind_rows() %>%
           distinct %>%
           as_tibble)
}

