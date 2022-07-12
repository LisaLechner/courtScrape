#' Return decisions from the European Court of Human Rights
#'
#' The function extracts decisions from the European Court of Human Rights archive \url{www.tribunalconstitucional.pt}
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
#' @importFrom dplyr select filter
#' @importFrom plyr ldply
#' @importFrom stringi stri_extract stri_pad stri_trim
#' @importFrom rvest read_html html_text html_nodes
#'
#' @keywords constitutional court, scraping
#'
#' @examples
#' out <- get_decisions_prt(date_start = "2019-06-01",
#' date_end = Sys.Date())
#' @export

get_decisions_echr <-  function(date_start = "2005-01-01",
                                date_end = Sys.Date(),
                                metadata = TRUE,
                                textdata = TRUE) {
  if (metadata == FALSE) {
    stop("metadata must be set TRUE")
  }
  # 1. First step is to downlaod all metadata
  if (metadata == TRUE) {
    base_url <-
      "http://hudoc.echr.coe.int/app/query/results?query=((((((((((((((((((((%20contentsitename%3AECHR%20AND%20(NOT%20(doctype%3DPR%20OR%20doctype%3DHFCOMOLD%20OR%20doctype%3DHECOMOLD)))%20XRANK(cb%3D14)%20doctypebranch%3AGRANDCHAMBER)%20XRANK(cb%3D13)%20doctypebranch%3ADECGRANDCHAMBER)%20XRANK(cb%3D12)%20doctypebranch%3ACHAMBER)%20XRANK(cb%3D11)%20doctypebranch%3AADMISSIBILITY)%20XRANK(cb%3D10)%20doctypebranch%3ACOMMITTEE)%20XRANK(cb%3D9)%20doctypebranch%3AADMISSIBILITYCOM)%20XRANK(cb%3D8)%20doctypebranch%3ADECCOMMISSION)%20XRANK(cb%3D7)%20doctypebranch%3ACOMMUNICATEDCASES)%20XRANK(cb%3D6)%20doctypebranch%3ACLIN)%20XRANK(cb%3D5)%20doctypebranch%3AADVISORYOPINIONS)%20XRANK(cb%3D4)%20doctypebranch%3AREPORTS)%20XRANK(cb%3D3)%20doctypebranch%3AEXECUTION)%20XRANK(cb%3D2)%20doctypebranch%3AMERITS)%20XRANK(cb%3D1)%20doctypebranch%3ASCREENINGPANEL)%20XRANK(cb%3D4)%20importance%3A1)%20XRANK(cb%3D3)%20importance%3A2)%20XRANK(cb%3D2)%20importance%3A3)%20XRANK(cb%3D1)%20importance%3A4)%20XRANK(cb%3D2)%20languageisocode%3AENG)%20XRANK(cb%3D1)%20languageisocode%3AFRE&select=sharepointid,Rank,itemid,docname,doctype,application,appno,conclusion,importance,originatingbody,typedescription,kpdate,kpdateAsText,documentcollectionid,documentcollectionid2,languageisocode,extractedappno,isplaceholder,doctypebranch,respondent,respondentOrderEng,ecli&sort=&rankingModelId=4180000c-8692-45ca-ad63-74bc4163871b"

    info <- readRDS("data/sysdata_echr.rds")
    info <- info[info$date >= date_start & info$date <= date_end,]

    # max_documents <- 161000 #current amount of documents (approx.)
    # page_nr <- seq(0,max_documents,by=500) #500 is the maximum number of items per request

    page_nr <- info$count
    pb_meta <- txtProgressBar(min = 0,
                              max = length(page_nr),
                              style = 3)
    meta <- lapply(seq_along(page_nr), function(p) {
      meta1 <- loadCache(key = list(p))
      if (!is.null(meta1)) {
        cat("Loaded cached data\n")
        return(meta1)
      }
      if (is.null(meta1)) {
        url  <- paste0(base_url, "&start=", page_nr[p], "&length=500")
        tryCatch({
          ht   <- read_html(url)
        },
        error = function(e) {
          e
        },
        warning = function(w) {
          cat("Was not able to retrieve information from page nr:",
              page_nr[p])
        })
        if (exists("ht")) {
          meta1 <- fromJSON(html_text(ht))
          meta1 <- unlist(meta1$results, recursive = FALSE)
          meta1 <-
            lapply(meta1, data.frame, stringsAsFactors = FALSE)
          meta1 <- ldply(meta1)
          meta1$`.id` <- NULL
          saveCache(meta1, key = list(p))
          return(meta1)
          Sys.sleep(6)
          if ((p %% 10) == 0) {
            Sys.sleep(60)
          }
        }
      }
      setTxtProgressBar(pb_meta, p, title = "Meta-data download")
    }) %>% ldply
    meta$date <-
      as.Date(
        stri_extract(regex = "[[:digit:]]{1,2}/[[:digit:]]{1,2}/[[:digit:]]{4}", meta$kpdate),
        "%m/%d/%Y"
      )
    meta <- meta[meta$date >= date_start & meta$date <= date_end,]
  }

  meta <- readRDS("data/echr_meta.RDS")

  if (textdata == TRUE) {
    if (exists("remDr")) {
      rm(remDr)
      gc()
    }
    if (exists("rD")) {
      rm(rD)
      gc()
    }
    eCaps <- list(chromeOptions =
                    list(
                      prefs = list(
                        "profile.default_content_settings.popups" = 0L,
                        "download.prompt_for_download" = FALSE,
                        "download.default_directory" = tempdir()
                      )
                    ))
    # set up selenium
    pb_text <- txtProgressBar(min = 0,
                              max = nrow(meta),
                              style = 3)
    texts <- lapply(2574:3237, function(m) {
      print(m)
      text <- loadCache(key = list(m, meta$itemid[m]))
      if (!is.null(text)) {
        cat("Loaded cached data\n")
        return(text)
      }
      if (is.null(text)) {
        base_url = "http://hudoc.echr.coe.int/app/conversion/docx/?library=ECHR&filename=please_give_me_the_document.docx&id="
        url1 <- paste0(base_url, meta$itemid[m])
        base_url = "http://hudoc.echr.coe.int/app/conversion/pdf/?library=ECHR&filename=please_give_me_the_document.pdf&id="
        url2 <- paste0(base_url, meta$itemid[m])
        portid <- 9513L
        #perma_url = "http://hudoc.echr.coe.int/eng?i="
        if (file.exists(paste0(tempdir(), "/down.docx"))) {
          unlink(paste0(tempdir(), "/down.docx"))
        }
        if (file.exists(paste0(tempdir(), "/down.pdf"))) {
          unlink(paste0(tempdir(), "/down.pdf"))
        }
        try(download.file(url1, paste0(tempdir(), "/down.docx"), mode = "wb"), silent =
              TRUE)
        if (!file.exists(paste0(tempdir(), "/down.docx"))) {
          try(download.file(url2, paste0(tempdir(), "/down.pdf"), mode = "wb"), silent =
                TRUE)
        }

        if (file.exists(paste0(tempdir(), "/down.pdf"))) {
          text <- readtext::readtext(paste0(tempdir(), "/down.pdf"))
          unlink(paste0(tempdir(), "/down.pdf"))
        }
        if (file.exists(paste0(tempdir(), "/down.docx"))) {
          text <- readtext::readtext(paste0(tempdir(), "/down.docx"))
          unlink(paste0(tempdir(), "/down.docx"))
        }

        if (!exists("text") | is.null(text)) {
          rD <-
            rsDriver(
              port = portid,
              browser = c("chrome"),
              extraCapabilities = eCaps,
              chromever = "78.0.3904.70"
            )
          remDr <- rD[["client"]]
          if (remDr$getStatus()$ready) {
            Sys.sleep(4)
            remDr$navigate(
              paste0(
                'https://hudoc.echr.coe.int/eng#{"itemid":["',
                meta$itemid[m],
                '"]}'
              )
            )
            Sys.sleep(4)

            webElem <-
              remDr$findElement(using = 'css selector', '#wordbutton')
            Sys.sleep(2)
            webElem$clickElement()
            Sys.sleep(3)
            if (stri_detect(regex = "\\.docx", list.files(tempdir())[1])) {
              text <-
                readtext::readtext(paste0(tempdir(), "/", list.files(tempdir())[1]))
            }

            if (!stri_detect(regex = "\\.docx", list.files(tempdir())[1])) {
              webElem <-
                remDr$findElement(using = 'css selector', '.officialLanguagesContainer a')
              Sys.sleep(1)
              webElem$clickElement()


              Sys.sleep(4)
              windows <- remDr$getWindowHandles()
              qpath <- sprintf("%s/session/%s/window",
                               remDr$serverURL,
                               remDr$sessionInfo[["id"]])
              remDr$queryRD(qpath, "POST", qdata = list(handle = windows[[2]]))
              Sys.sleep(4)

              webElem <-
                remDr$findElement(using = 'css selector', '#wordbutton')
              webElem$clickElement()

              Sys.sleep(5)
              if (!stri_detect(regex = "\\.docx", list.files(tempdir())[1])) {
                text <- data.frame(text = NA)
              }

              if (stri_detect(regex = "\\.docx", list.files(tempdir())[1])) {
                if (is.null(text) | !exists("text")) {
                  text <-
                    readtext::readtext(paste0(tempdir(), "/", list.files(tempdir())[1]))
                }
                unlink(list.files(tempdir(), full.names = TRUE)[1])
              }
            }
            Sys.sleep(1)
            rm(remDr, rD, webElem, qpath, windows)
            gc()
          } else{
            text <- data.frame(text = NA)
          }
          if (exists("remDr")) {
            rm(remDr)
          }
          if (exists("rD")) {
            rm(rD)
            gc()
          }

          if (!exists("text") | is.null(text)) {
            text <- data.frame(text = NA)
          }

        }
        text$doc_id <- meta$itemid[m]


        saveCache(text, key = list(m, meta$itemid[m]))
        return(text)
        Sys.sleep(2)
        if ((m %% 200) == 0) {
          Sys.sleep(10)
          portid <- portid + 1
        }
      }
      setTxtProgressBar(pb_text, m, title = "Text-data download")
    }) %>% ldply()
  }

  if (textdata == FALSE & metadata == TRUE) {
    temp <- meta
  }

  if (textdata == TRUE & metadata == TRUE) {
    temp <- merge(meta, texts)
  }
  return(temp)
}
