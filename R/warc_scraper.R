#' Logs a selenium remote driver instance into WARC
#'
#' @param username
#' @param password
#' @param articles
#' @param batch_size How many papers to download before logging in again
#' @param pause_batch Seconds to pause between logins
#' @param pause_article Seconds to pause between individual pdf downloads
#'
#' @return
#' @export
#'
#' @examples
warc_download_pdf <- function(articles, username = NULL, password = NULL, batch_size = 9, pause_batch = 1600, pause_article = 20){

  message("Connecting to Selenium...")

  output_dir <- "C:/Users/neild/Downloads"  # Replace with your desired directory
  output_file <- "saved_page.html"  # Desired filename for the HTML file
  full_path <- file.path(output_dir, output_file)


  fprof <- RSelenium::makeFirefoxProfile(list(browser.download.dir = "/home/seluser/downloads",
                                              browser.download.folderList = 2L,
                                              browser.download.manager.showWhenStarting = FALSE,
                                              browser.helperApps.neverAsk.openFile = "text/csv",
                                              browser.helperApps.neverAsk.saveToDisk = "text/csv",
                                              browser.download.folderList = 2,
                                              browser.download.dir = output_dir,
                                              browser.helperApps.neverAsk.saveToDisk = "text/html",
                                              browser.download.useDownloadDir = TRUE,
                                              print.always_print_silent = TRUE,
                                              print.show_print_progress = FALSE,
                                              print.save_print_settings = TRUE,
                                              print.print_to_file = TRUE,
                                              print.print_to_filename = "C:/Users/neild/Downloads/output.pdf")
  )

  # Interactive browser for debugging
  driver <- RSelenium::rsDriver(
    browser = "firefox",
    verbose = FALSE,
    port = 4545L,
    chromever = NULL,
    extraCapabilities = fprof
  )

  remDr <- driver$client

  articles_batches <- articles |>
    dplyr::mutate(batch_group = (dplyr::row_number() - 1) %/% batch_size + 1)

  for(i in 1:max(articles_batches$batch_group)){
    remDr$open(silent = TRUE)
    remDr <- warc_login(remDr, username, password)

    current_batch <- articles_batches |>
      dplyr::filter(batch_group==i) |>
      dplyr::pull(url) |>
      purrr::walk(
        .f = ~{
          remDr$navigate(.)
          Sys.sleep(5)
          webElem <- remDr$findElement(using = 'xpath', value = '/html/body/main/section/div/div/div[2]/a')
          download_link <- webElem$getElementAttribute("href")
          Sys.sleep(pause_article)

          # Download the file
          remDr$executeScript("
          let link = document.createElement('a');
          link.href = document.location.href;
          link.download = arguments[0];
          link.click();
        ", list(glue::glue('{stringr::str_replace(stringr::str_replace(., "https://www.warc.com/content/article/", ""), "/", "-")}.html')))
          Sys.sleep(pause_article + runif(1) * pause_article)
          message(glue::glue("Downloaded {.}"))
        }
      )

    remDr$close()
    Sys.sleep(pause_batch + runif(1) * pause_batch)
  }

}

#' Logs a selenium remote driver instance into WARC
#'
#' @param remDr
#' @param username
#' @param password
#'
#' @return
#' @export
#'
#' @examples
warc_login <- function(remDr = NULL, username = NULL, password = NULL){

  message("Navigating to Warc...")

  remDr$navigate("https://warc.com/")

  Sys.sleep(2)

  message("Logging in...")

  webElem <- remDr$findElement(using = 'xpath', value = '//*[@id="header-interaction"]/div[1]/a[1]')
  webElem$clickElement()

  Sys.sleep(2)
  webElem <- remDr$findElement(using = 'xpath', value = '//*[@id="username"]')
  webElem$sendKeysToElement(list(username))

  webElem <- remDr$findElement(using = 'xpath', value = '//*[@id="password"]')
  webElem$sendKeysToElement(list(password))

  webElem <- remDr$findElement(using = 'xpath', value = '//*[@id="react-root"]/div/form/div[1]/div[3]/button/span')
  webElem$clickElement()

  Sys.sleep(10)

  remDr

}

#' Pages through search results and grabs article URLs
#'
#' @param source_page
#' @param username
#' @param password
#' @param max_results_pages limit of results pages to work through
#'
#' @return A tibble of article URLs
#' @export
#'
#' @examples
scrape_warc_search_urls <- function(source_page = "https://www.warc.com/search?sc=IPA%2520%2528UK%2529&t=case%2520studies",
                             username = NULL,
                             password = NULL,
                             max_results_pages = 200){

  message("Connecting to Selenium...")

  output_dir <- "C:/Users/neild/Downloads"  # Replace with your desired directory
  output_file <- "saved_page.html"  # Desired filename for the HTML file
  full_path <- file.path(output_dir, output_file)


  fprof <- RSelenium::makeFirefoxProfile(list(browser.download.dir = "/home/seluser/downloads",
                                              browser.download.folderList = 2L,
                                              browser.download.manager.showWhenStarting = FALSE,
                                              browser.helperApps.neverAsk.openFile = "text/csv",
                                              browser.helperApps.neverAsk.saveToDisk = "text/csv",
                                              browser.download.folderList = 2,
                                              browser.download.dir = output_dir,
                                              browser.helperApps.neverAsk.saveToDisk = "text/html",
                                              browser.download.useDownloadDir = TRUE,
                                              print.always_print_silent = TRUE,
                                              print.show_print_progress = FALSE,
                                              print.save_print_settings = TRUE,
                                              print.print_to_file = TRUE,
                                              print.print_to_filename = "C:/Users/neild/Downloads/output.pdf")
  )

  # Interactive browser for debugging
  driver <- RSelenium::rsDriver(
    browser = "firefox",
    verbose = FALSE,
    port = 4545L,
    chromever = NULL,
    extraCapabilities = fprof
  )

  remDr <- driver$client

  remDr$open(silent = TRUE)

  remDr <- warc_login(remDr, username, password)

  message("Navigating to results page...")

  remDr$navigate(source_page)

  Sys.sleep(10)

  all_results <- tibble::tibble(url = character())

  finished <- FALSE
  loop_count <- 1 #counter to avoid infinite loop just in case

  while(!finished & loop_count <= max_results_pages){

    message(glue::glue("Scraping results page: {loop_count}"))

    link_elements <- NULL
    link_urls <- NULL

    link_elements <- remDr$findElements(
      using = "css selector",
      value = "#searchResultsBody a" # Select all <a> tags inside the div with id "example-div"
    )

    link_urls <- tibble::tibble(url = unlist(sapply(link_elements, function(link) link$getElementAttribute("href"))))

    # Next page button is disabled on the last page so clicking it will do nothing. Check if we already have these results.
    finished <- all(link_urls$url %in% all_results$url)

    all_results <- all_results |>
      dplyr::bind_rows(link_urls)

    # Click next page
    remDr$executeScript("window.scrollTo(0, document.body.scrollHeight);")
    webElem <- remDr$findElement(using = 'xpath', value = '/html/body/main/div[2]/div/div[3]/section/div/div[5]/nav/ul/li[4]')
    # Not sure why we need to click it twice but we do
    webElem$clickElement()
    Sys.sleep(1)
    webElem$clickElement()

    loop_count <- loop_count+1

    Sys.sleep(5+runif(1)*5)
  }

  remDr$close()

  all_results |>
    dplyr::group_by(url) |>
    dplyr::summarise() |>
    dplyr::filter(stringr::str_detect(url, "article"))
}

# Local browser connection because it's easier to debug
connect_selenium_warc <- function(){

  output_dir <- "C:/Users/neild/Downloads"  # Replace with your desired directory
  output_file <- "saved_page.html"  # Desired filename for the HTML file
  full_path <- file.path(output_dir, output_file)


  fprof <- RSelenium::makeFirefoxProfile(list(browser.download.dir = "/home/seluser/downloads",
                                              browser.download.folderList = 2L,
                                              browser.download.manager.showWhenStarting = FALSE,
                                              browser.helperApps.neverAsk.openFile = "text/csv",
                                              browser.helperApps.neverAsk.saveToDisk = "text/csv",
                                              browser.download.folderList = 2,
                                              browser.download.dir = output_dir,
                                              browser.helperApps.neverAsk.saveToDisk = "text/html",
                                              browser.download.useDownloadDir = TRUE,
                                              print.always_print_silent = TRUE,
                                              print.show_print_progress = FALSE,
                                              print.save_print_settings = TRUE,
                                              print.print_to_file = TRUE,
                                              print.print_to_filename = "C:/Users/neild/Downloads/output.pdf")
  )

  # Interactive browser for debugging
  driver <- RSelenium::rsDriver(
    browser = "firefox",
    verbose = FALSE,
    port = 4545L,
    chromever = NULL,
    extraCapabilities = fprof
  )

  remDr <- driver$client

  remDr
}
