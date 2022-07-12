#' Return decisions from the European Court of Justice
#'
#' The function extracts decisions from the European Court of Justice archive \url{http://curia.europa.eu/}
#'
#' @param date_start an object of class \code{character} or \code{date} with the format yyyy-mm-dd indicating from
#' which date on data should be retrieved.
#' @param date_end an object of class \code{character} or \code{date} with the format yyyy-mm-dd indicating the date until
#' data should be retrieved.
#' @param metadata a logical object indicating whether metadata such as case ID, decision date, etc. should be retrieved or not. The default is \code{TRUE}.
#' @param textdata a logical object indicating whether the fulltexts of the court decisions should be retrieved or not. The default is \code{TRUE}.
#'
#' @return an object of class \code{data.frame} which contains the downloaded decisions and
#' if required the \code{metadata} and \code{textdata}
#'
#' @importFrom plyr ldply
#' @importFrom readtext
#' @importFrom stringi stri_extract
#' @importFrom rvest html_nodes read_html html_attr html_text
#' @importFrom xml2 xml_attrs
#' @importFrom R.cache
#'
#' @keywords constitutional court, scraping
#'
#' @examples
#' out <- get_decisions_aut(date_start = "2019-06-01",
#' date_end = Sys.Date())
#' @export

get_decisions_ecj <-  function(date_start = "2005-01-01",
                               date_end = Sys.Date(),
                               metadata = TRUE,
                               textdata = TRUE,
                               select_lang = c("en", "fr")) {
  date_start <- format(as.Date(date_start), "%Y.%m.%d")
  date_end <- format(as.Date(date_end), "%Y.%m.%d")
  url <-
    paste0(
      "http://curia.europa.eu/juris/documents.jsf?pro=&nat=or&oqp=&dates=%2524type%253Dpro%2524mode%253D1M%2524from%253D",
      date_start,
      "%2524to%253D",
      date_end,
      "&lg=&language=de&jur=C%2CT%2CF&cit=none%252CC%252CCJ%252CR%252C2008E%252C%252C%252C%252C%252C%252C%252C%252C%252C%252Ctrue%252Cfalse%252Cfalse&td=%3B%3B%3BPUB1%3B%3B%3BORDALL&pcs=Oor&avg=&page=1&mat=or&jge=&for=&cid=155497"
    )
  ht <- read_html(url)

  xpath <-
    "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'pagination', ' ' ))]"
  page_nr <-
    html_nodes(ht, xpath = xpath) %>% html_text() %>% stri_extract(., regex =
                                                                     "[[:digit:].]{1,} [[:alpha:].]{1,}") %>% gsub(" [[:alpha:].]{1,}", "", .) %>% as.numeric()
  pb <- txtProgressBar(min = 0, max = page_nr, style = 3)

  output <- lapply(1:page_nr, function(p) {
    print(p)
    temp <- loadCache(key = list(p, date_start, date_end))
    if (!is.null(temp)) {
      cat("Loaded cached data\n")
      return(temp)
    }
    if (is.null(temp)) {
      Sys.sleep(2)
      url <-
        paste0(
          "http://curia.europa.eu/juris/documents.jsf?pro=&nat=or&oqp=&dates=%2524type%253Dpro%2524mode%253D1M%2524from%253D",
          date_start,
          "%2524to%253D",
          date_end,
          "&lg=&language=de&jur=C%2CT%2CF&cit=none%252CC%252CCJ%252CR%252C2008E%252C%252C%252C%252C%252C%252C%252C%252C%252C%252Ctrue%252Cfalse%252Cfalse&td=%3B%3B%3BPUB1%3B%3B%3BORDALL&pcs=Oor&avg=&page=",
          p,
          "&mat=or&jge=&for=&cid=155497"
        )
      ht <- read_html(url)

      #html urls
      node <- html_nodes(ht, xpath = "//*[(@id = 'docHtml')]//a")
      urls <- unique(html_attr(node, "href"))
      if (length(urls) > 0) {
        urls2 <- data.frame(
          urls = urls,
          docid = gsub(
            "docid=",
            "",
            stri_extract(urls, regex = "docid=[[:digit:]]{1,}")
          ),
          lang = gsub(
            "lang=",
            "",
            stri_extract(urls, regex = "lang=[[:alpha:]]{1,}")
          ),
          format = "html"
        )
      }
      if (length(urls) == 0) {
        urls2 <- data.frame(
          urls = NA,
          docid = NA,
          lang = NA,
          format = "html"
        )
      }

      #pdf urls
      node2 <- html_nodes(ht, "#docPdf .tooltipLink")
      urls2 <- unique(html_attr(node2, "href"))
      if (length(urls2) > 0) {
        urls2 <- data.frame(
          urls = urls2,
          docid = gsub(
            "docid=",
            "",
            stri_extract(urls2, regex = "docid=[[:digit:]]{1,}")
          ),
          lang = toupper(gsub(
            "lang=",
            "",
            stri_extract(urls2, regex = "lang=[[:alpha:]]{1,}")
          )),
          format = "pdf"
        )
        urls2 <- urls2[!is.na(urls2$urls), ]
        if (is.data.frame(urls)) {
          urls2 <- urls2[!urls2$docid %in% urls$docid, ]
        }
      }
      if (length(urls2) == 0) {
        urls2 <- data.frame(
          urls = NA,
          docid = NA,
          lang = NA,
          format = "html"
        )
      }
      urls <- rbind(urls, urls2)
      urls$lang <- as.character(urls$lang)
      #urls <- urls[stri_detect_regex(urls$lang,"[[:UPPER:]]{1,}"),]
      urls$lang <- toupper(urls$lang)
      urls$docid <- factor(urls$docid, levels = unique(urls$docid))
      docid <- unique(urls$docid)
      urls <-
        split(as.character(urls$urls),
              list(urls$lang, urls$docid, urls$format))


      if (metadata == TRUE) {
        Sys.sleep(1)
        #docid
        docid <- docid
        # case nr
        xpath <-
          "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'table_document_ligne', ' ' ))]//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'table_cell_aff', ' ' )) and (((count(preceding-sibling::*) + 1) = 1) and parent::*)]"
        node <- html_nodes(ht, xpath = xpath)
        case <-
          html_text(node) %>% stri_trim(.)  %>% gsub("\\s+", " ", .)
        # ecli id
        xpath <-
          "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'table_document_ligne', ' ' ))]//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'table_cell_doc', ' ' ))]"
        node <- html_nodes(ht, xpath = xpath)
        ecli_id = html_text(node) %>% stri_trim(.) %>% stri_extract(., regex =
                                                                      "ECLI.+")  %>% gsub("\\s+", " ", .)
        Sys.sleep(1)
        # date
        xpath <-
          "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'table_document_ligne', ' ' ))]//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'table_cell_date', ' ' ))]"
        node <- html_nodes(ht, xpath = xpath)
        date <-
          html_text(node) %>% stri_trim(.)  %>% gsub("\\s+", " ", .)
        Sys.sleep(1)
        # party
        xpath <-
          "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'table_document_ligne', ' ' ))]//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'table_cell_nom_usuel', ' ' ))]"
        node <- html_nodes(ht, xpath = xpath)
        party <-
          html_text(node) %>% stri_trim(.) %>% gsub("\\s+", " ", .)
        # subject
        Sys.sleep(1)
        xpath <-
          "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'table_document_ligne', ' ' ))]//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'table_cell_links_curia', ' ' ))]"
        node <- html_nodes(ht, xpath = xpath)
        subject <-
          html_text(node) %>% stri_trim(.) %>% stri_extract(., regex = ".+\\n") %>%  gsub("\\n", "", .)

        docid <- docid[!is.na(docid)]
        meta <- data.frame(docid = docid)

        if (nrow(meta) > 0) {
          if (length(case) == nrow(meta)) {
            meta$case <- case
          } else
            (
              meta$case <- NA
            )

          if(length(date)==nrow(meta)){
            meta$date <- date
          }else(
            meta$date <- NA
          )

          if(length(party)==nrow(meta)){
            meta$party <- party
          }else(
            meta$party <- NA
          )


          if(length(subject)==nrow(meta)){
            meta$subject <- subject
          }else(
            meta$subject <- NA
          )
        } else{
          warning("Metadata could not be fully downloaded due to missing entries.")
          metadata = FALSE
        }
      }
      if (textdata == TRUE) {
        lang <- toupper(select_lang)

        texts <- lapply(lang, function(l) {
          urls1 <- urls[stri_detect_regex(names(urls), l)]
          texts1 <- lapply(seq_along(urls1), function(u) {
            url <-  urls1[[u]]
            if (length(url) == 0) {
              text <-
                data.frame(
                  text = "",
                  docid = stri_extract(names(urls1)[u], regex = "[[:digit:]]{1,}"),
                  lang = l
                )
            } else{
              if (length(url) > 1) {
                url <- url[1]
              }
              if (stri_detect(names(urls1)[u], regex = "html")) {
                text <- read_html(url) %>% html_text()
                text <-
                  data.frame(
                    text = as.character(text),
                    docid = stri_extract(names(urls1)[u], regex = "[[:digit:]]{1,}"),
                    lang = l
                  )
              }
              if (stri_detect(names(urls1)[u], regex = "pdf")) {
                try(download.file(url, paste0(tempdir(), "/down.pdf"), mode = "wb"),
                    silent = TRUE)
                if (!file.exists(paste0(tempdir(), "/down.pdf"))) {
                  text <-
                    data.frame(
                      text = "",
                      docid = stri_extract(names(urls1)[u], regex = "[[:digit:]]{1,}"),
                      lang = l
                    )
                }
                if (file.exists(paste0(tempdir(), "/down.pdf"))) {
                  text <- readtext::readtext(paste0(tempdir(), "/down.pdf"))
                  unlink(paste0(tempdir(), "/down.pdf"))
                  text <-
                    data.frame(
                      text = as.character(text$text),
                      docid = stri_extract(names(urls1)[u], regex = "[[:digit:]]{1,}"),
                      lang = l
                    )
                }
              }

              Sys.sleep(2)

            }
            return(text)
          }) %>% ldply
          return(texts1)
        }) %>% ldply
      }
      if (textdata == FALSE & metadata == TRUE) {
        temp <- meta
      }

      if (textdata == TRUE & metadata == FALSE) {
        temp <- texts
      }

      if (textdata == TRUE & metadata == TRUE) {
        temp <- merge(meta, texts)
      }

      if (textdata == FALSE & metadata == FALSE) {
        temp <- data.frame(urls = urls)
      }
      setTxtProgressBar(pb, p)

      Sys.sleep(3)
      saveCache(temp, key = list(p, date_start, date_end))
      if ((p %% 2) == 0) {
        Sys.sleep(10)
      }
      return(temp)
    }
  }) %>% ldply
}
