#' Return decisions from the Austrian Constitutional Court
#'
#' The function extracts opinions from the European Constitutional Court archive \url{https://curia.europa.eu}
#'
#' @param date_start an object of class \code{character} or \code{date} with the format yyyy-mm-dd indicating from
#' which date on data should be retrieved.
#' @param date_end an object of class \code{character} or \code{date} with the format yyyy-mm-dd indicating the date until
#' data should be retrieved.
#' @param select_lang allows for the selection of languages to be downloaded.
#' At least some opinions for languages spoken in the European Union should be available. Please use iso2c.
#' English and French are the default languages as most opinions exist for these.
#'
#' @return an object of class \code{data.frame} which contains the downloaded decisions and
#' if required the \code{metadata} and \code{textdata}
#'
#' @importFrom plyr ldply
#' @importFrom readtext
#' @importFrom stringi stri_extract
#' @importFrom rvest html_nodes read_html html_attr html_text
#' @importFrom xml2 xml_attrs
#' @importFrom utils txtProgressBar
#' @importFrom R.Cache loadCache
#'
#' @keywords constitutional court, scraping
#'
#' @examples
#' out <- get_opinions_ejc(date_start = "2019-06-01",
#' date_end = Sys.Date())
#'
#' @export get_opinions_ecj

.get_html_page_ecj <- function(date_start, date_end, page = 1, language = "de") {
  date_start <- format(as.Date(date_start), "%Y.%m.%d")
  date_end <- format(as.Date(date_end), "%Y.%m.%d")
  url <-
    paste0(
      "https://curia.europa.eu/juris/documents.jsf?page=",
      page,
      "&oqp=&for=&mat=or&lgrec=de&jge=&td=%24mode%3D1Y%24from%3D",
      date_start,
      "%24to%3D",
      date_end,
      "%3B%3B%3BPUB3%3B%3B%3B%3BORDALL&jur=C%2CT%2CF&dates=&pcs=Oor&lg=&pro=&nat=or&cit=none%252CC%252CCJ%252CR%252C2008E%252C%252C%252C%252C%252C%252C%252C%252C%252C%252Ctrue%252Cfalse%252Cfalse&language=",
      language,
      "&avg=&cid=40006865"
    )
  html <- read_html(url)
}

.get_number_pages_ecj <- function(html) {
  xpath <-
    "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'pagination', ' ' ))]"
  page_nr <-
    html_nodes(html, xpath = xpath) %>% html_text() %>% stri_extract(., regex =
                                                                       "[[:digit:].]{1,} [[:alpha:].]{1,}") %>% gsub(" [[:alpha:].]{1,}", "", .) %>% as.numeric()
  return(page_nr)
}

.get_html_urls_ecj <- function(ht) {
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
  return(urls2)
}

.get_pdf_urls_ecj <- function(ht) {
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
  }
  if (length(urls2) == 0) {
    urls2 <- data.frame(
      urls = NA,
      docid = NA,
      lang = NA,
      format = "html"
    )
  }
  return(urls2)
}

.get_urls_ecj <- function(ht, select_lang, date_start, date_end, p) {
  #this function gets URLS from html and pdf files and removes unnecessary pdfs
  html_urls <- .get_html_urls_ecj(ht)
  lang <- toupper(select_lang)
  #here we need to change language of the website to download pdfs in all languages
  pdf_urls <- lapply(lang, function(lang) {
    ht <- .get_html_page_ecj(date_start, date_end, page = p, language = lang)
    urls <- .get_pdf_urls_ecj(ht)
  }) %>% ldply
  if (is.data.frame(html_urls)) {
    pdf_urls <- pdf_urls[!pdf_urls$docid %in% html_urls$docid, ]
  }
  urls <- rbind(html_urls, pdf_urls)
  #NEW
  urls <-na.omit(urls)
  return(urls)
}

.get_meta_data_ecj <- function(ht, docid) {
  #Sys.sleep(1)
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
  #Sys.sleep(1)
  # date
  xpath <-
    "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'table_document_ligne', ' ' ))]//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'table_cell_date', ' ' ))]"
  node <- html_nodes(ht, xpath = xpath)
  date <-
    html_text(node) %>% stri_trim(.)  %>% gsub("\\s+", " ", .)
  xpath <-
    "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'table_document_ligne', ' ' ))]//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'table_cell_nom_usuel', ' ' ))]"
  node <- html_nodes(ht, xpath = xpath)
  party <-
    html_text(node) %>% stri_trim(.) %>% gsub("\\s+", " ", .)
  xpath <-
    "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'table_document_ligne', ' ' ))]//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'table_cell_links_curia', ' ' ))]"
  node <- html_nodes(ht, xpath = xpath)
  subject <-
    html_text(node) %>% stri_trim(.) %>% stri_extract(., regex = ".+\\n") %>%  gsub("\\n", "", .)
  meta <- data.frame(docid = docid)

  if (nrow(meta) > 0) {
    if (length(case) == nrow(meta)) {
      meta$case <- case
    } else
      (meta$case <- NA)

    #ADD ECLI
    if (length(ecli_id) == nrow(meta)) {
      meta$ecli <- ecli_id
    }
    # END
    if (length(date) == nrow(meta)) {
      meta$date <- date
    } else
      (
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
  return(meta)
}

.download_textdata_ecj <- function(select_lang, urls) {
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
          try(download.file(url, paste0(tempdir(), "/down.pdf"), quiet = T, mode = "wb"),
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
        Sys.sleep(0.5)
      }
      return(text)
    }) %>% ldply
    return(texts1)
  }) %>% ldply
}

.get_docid_list_in_order_ecj <- function(urls, languages) {
  #this function returns a list of docids in correct order to match with metadata if possible
  docids <- unique(urls$docid)
  #check if all htmls available
  html_urls <- urls[urls$format=="html", ]
  html_docids <- unique(html_urls$docid)
  if(length(html_docids) == length(docids)) {
    return(html_docids)
  }
  for(lang in languages) {
    pdf_urls_lang <- urls[urls$lang == lang & urls$format == "pdf", ]
    pdf_lang_docids <- unique(pdf_urls_lang$docid)
    if(length(pdf_lang_docids) == length(docids)) {
      return(pdf_lang_docids)
    }
  }
  return(NULL)
}

.modify_urls_ecj <- function(urls) {
  urls$lang <- as.character(urls$lang)
  urls <- urls[stri_detect_regex(urls$lang,"[[:UPPER:]]{1,}"),]
  urls$lang <- toupper(urls$lang)
  urls$docid <- factor(urls$docid, levels = unique(urls$docid))
  docid <- unique(urls$docid)
  urls <-
    split(as.character(urls$urls),
          list(urls$lang, urls$docid, urls$format))
  return(urls)
}

get_opinions_ecj <- function( date_start  = "2005-01-01",
                           date_end    = Sys.Date(),
                           select_lang = c("en", "fr")) {

  ht <- .get_html_page_ecj(date_start, date_end)
  n_pages <- .get_number_pages_ecj(ht)
  pb <- txtProgressBar(min = 0, max = n_pages, style = 3)
  output <- lapply(1:n_pages, function(p) {
    ht <- .get_html_page_ecj(date_start, date_end, page = p)
    urls <- .get_urls_ecj(ht, date_start = date_start, date_end = date_end, p = p, select_lang = select_lang)
    docid <- .get_docid_list_in_order_ecj(urls, select_lang)
    urls <- .modify_urls_ecj(urls)
    texts <- .download_textdata_ecj(select_lang, urls)
    setTxtProgressBar(pb, p)
    if(!is.null(docid)) {
      meta <- .get_meta_data_ecj(ht, docid)
      return(merge(meta, texts))
    } else {
      return(texts)
    }
  }) %>% ldply
  return(output)
}
