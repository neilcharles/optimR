#' Downloads a scenario from Media Mix Navigator
#'
#' Note: Exclude 'Please Choose...' from the item count
#' @param category_option Item number in the dropdown list (All Categories is #1)
#' @param appeal_option Item number in the dropdown list
#' @param percent_sales_online_option Item number in the dropdown list
#' @param brand_size Brand size in £m
#' @param budget Budget in £m
#' @param risk_option Item number in the dropdown list
#' @param remDr A selenium connection, usually made with connect_selenium()
#'
#' @return NULL (a zip file will save to the download directory)
#' @export
#'
#' @examples
scrape_navigator <- function(category_option = NULL,
                             appeal_option = NULL,
                             percent_sales_online_option = NULL,
                             brand_size = NULL,
                             budget = NULL,
                             risk_option = NULL,
                             remDr = NULL){

  message("Connecting to Selenium...")

  remDr$open(silent = TRUE)

  message("Navigating to Media Mix Navigator...")

  remDr$navigate("https://thinkbox-tv.netlify.app/iframes/mediamixnavigator.html")

  Sys.sleep(2)

  message("Setting options...")

  # ----- Category -------------------------------------------------------------
  webElem <- remDr$findElement(using = 'xpath', value = glue::glue('//*[@id="category"]'))
  webElem$clickElement()
  webElem <- remDr$findElement(using = 'xpath', value = glue::glue('//*[@id="category"]/option[{category_option+1}]'))
  webElem$clickElement()

  # ----- Appeal ---------------------------------------------------------------
  webElem <- remDr$findElement(using = 'xpath', value = glue::glue('//*[@id="appeal"]'))
  webElem$clickElement()
  webElem <- remDr$findElement(using = 'xpath', value = glue::glue('//*[@id="appeal"]/option[{appeal_option+1}]'))
  webElem$clickElement()

  # ----- Online Sales ---------------------------------------------------------
  webElem <- remDr$findElement(using = 'xpath', value = glue::glue('//*[@id="percent-sales"]'))
  webElem$clickElement()
  webElem <- remDr$findElement(using = 'xpath', value = glue::glue('//*[@id="percent-sales"]/option[{percent_sales_online_option+1}]'))
  webElem$clickElement()

  # ----- Brand Size -----------------------------------------------------------
  webElem <- remDr$findElement(using = 'xpath', value = '//*[@id="brand-size"]')
  webElem$clickElement()
  webElem <- remDr$findElement(using = 'xpath', value = '//*[@id="brand-size"]')
  webElem$sendKeysToElement(list(key = "backspace", key = "backspace", as.character(brand_size), key = "enter"))

  # ----- Budget ---------------------------------------------------------------
  webElem <- remDr$findElement(using = 'xpath', value = '//*[@id="budget-size"]')
  webElem$clickElement()
  webElem <- remDr$findElement(using = 'xpath', value = '//*[@id="budget-size"]')
  webElem$sendKeysToElement(list(key = "backspace", key = "backspace", as.character(budget), key = "enter"))

  # ----- Fixed to revenue -----------------------------------------------------
  webElem <- remDr$findElement(using = 'xpath', value = glue::glue('//*[@id="outputs"]'))
  webElem$clickElement()
  webElem <- remDr$findElement(using = 'xpath', value = glue::glue('//*[@id="outputs"]/option[2]'))
  webElem$clickElement()

  # ----- Risk -----------------------------------------------------------------
  webElem <- remDr$findElement(using = 'xpath', value = glue::glue('//*[@id="minimise-risk"]'))
  webElem$clickElement()
  webElem <- remDr$findElement(using = 'xpath', value = glue::glue('//*[@id="minimise-risk"]/option[{risk_option+1}]'))
  webElem$clickElement()

  # ----- Download -------------------------------------------------------------
  Sys.sleep(2)

  webElem <- remDr$findElement(using = 'xpath', value = '//*[@id="demand_growth_download"]')
  webElem$clickElement()

  Sys.sleep(5)

  message("Saved results")

  remDr$close()
}

#' Helper that prints a docker run command string to the console
#'
#' @return NULL
#' @export
#'
#' @examples
selenium_docker_string <- function(){
  message('Swap "/c/temp" for your directory of choice in the following string and then run on the command line.')
  message('docker run -d --name seleniumserver -p 4445:4444 -v "/c/temp:/home/seluser/downloads" selenium/standalone-firefox"')
}

#' Connects to selenium
#'
#' @return A selenium connection
#' @export
#'
#' @examples
connect_selenium <- function(){

  fprof <- RSelenium::makeFirefoxProfile(list(browser.download.dir = "/home/seluser/downloads",
                                              browser.download.folderList = 2L,
                                              browser.download.manager.showWhenStarting = FALSE,
                                              browser.helperApps.neverAsk.openFile = "text/csv",
                                              browser.helperApps.neverAsk.saveToDisk = "text/csv")
  )

  remDr <- RSelenium::remoteDriver(remoteServerAddr = "localhost"
                                   , port = 4445L
                                   , browserName = "firefox"
                                   , extraCapabilities = fprof)

  remDr
}


#' Builds a set of budget scenarios to extract from Media Mix Navigator
#'
#' @param min_budget minimum budget
#' @param max_budget maximum budget
#' @param step_budget step size between min_budget and max_budget
#' @param category_option Item number in the dropdown list (All Categories is #1)
#' @param appeal_option Item number in the dropdown list
#' @param percent_sales_online_option Item number in the dropdown list
#' @param brand_size brand size in £m
#' @param risk_option Item number in the dropdown list
#'
#' @return a tibble of budget scenarios
#' @export
#'
#' @examples
make_scenarios <- function(min_budget = 1, max_budget = 10, step_budget = 1,
                           category_option = 1,
                           appeal_option = 1,
                           percent_sales_online_option = 1,
                           brand_size = 100,
                           risk_option = 1){

  n_budget <- ceiling((max_budget +1 - min_budget) / step_budget)

  tibble::tibble(
    category_option = rep(category_option, n_budget),
    appeal_option = rep(appeal_option, n_budget),
    percent_sales_online_option = rep(percent_sales_online_option, n_budget),
    brand_size = rep(brand_size, n_budget),
    budget = seq(min_budget, max_budget, step_budget),
    risk_option = rep(risk_option, n_budget)
  )
}
