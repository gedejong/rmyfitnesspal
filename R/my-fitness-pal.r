base_url <- "https://www.myfitnesspal.com/"
base_api_url <- "https://api.myfitnesspal.com/"
login_path <- "account/login"

#' Retrieve a MFP (myfitnesspal) context object to retrieve authenticated data from myfitnesspal.
#'
#' @param username Username of the account. Not the email address, but the username found in your profile.
#' @param password Password of the account
#' 
#' @title Get context for MFP
#' @export
#' @importFrom grDevices rgb
#' @return List containing authentication and user data
mfp_context <- function(username, password)
{
    login_url <- paste0(base_url, login_path, sep="")
    response <- httr::GET(
        login_url,
        httr::add_headers(
                 "user-agent" = "Mozilla/5.0",
                 "Cache-Control" = "no-cache"))
    document <- httr::content(response, "parsed")
    auth_token <- xml2::xml_text(xml2::xml_find_first(document, "(//input[@name='authenticity_token']/@value)[1]"))
    utf8_field <- xml2::xml_text(xml2::xml_find_first(document, "(//input[@name='utf8']/@value)[1]"))
    response2 <- httr::POST(login_url, body=list(
                "utf8" = utf8_field,
                "authenticity_token" = auth_token,
                "username" = username,
                "password" = password),
        httr::add_headers("user-agent" = "Mozilla/5.0", "Cache-Control" = "no-cache"))
    if (httr::http_status(response2)$category != "Success")
        stop("Incorrect username or password")

    parsed_response <- httr::content(response2, "text")
    if (length(grep("Incorrect username or password", parsed_response)))
        stop("Incorrect username or password")

    context <- c()
    context$auth_data <- get_auth_data()
    context$user_data <- get_user_data(context)
    context
}

#' Retrieves authentication data
#'
#' Retrieves authentication data from MFP, using the session cookie after a succesful login.
#' @return List structure containing authentication data.
#' @author Edwin De Jong
get_auth_data <- function() {
    result <- get_request_for_url(
            paste0(base_url, "user/auth_token", "?refresh=true"), hdrs=c("Accept" = "application/json"))
    if (httr::http_status(result)$category != "Success")
        stop(paste("Unable to fetch authentication token from MyFitnessPal: "))

    httr::content(result, "parsed")
}

#' Retrieves user data from MyFitnessPall
#'
#' Given a context object, retrieves user data.
#' @title Retrieve user data
#' @param context Context object containing authentication data
#' @return List containing user data
#' @author Edwin De Jong
get_user_data <- function(context) {
    requested_fields <- c(
        "diary_preferences", "goal_preferences", "unit_preferences",
        "paid_subscriptions", "account", "goal_displays",
        "location_preferences", "system_data", "profiles",
        "step_sources")

    fields_part <-  "fields%5B%5D=diary_preferences&fields%5B%5D=goal_preferences&fields%5B%5D=unit_preferences&fields%5B%5D=paid_subscriptions&fields%5B%5D=account&fields%5B%5D=goal_displays&fields%5B%5D=location_preferences&fields%5B%5D=system_data&fields%5B%5D=profiles&fields%5B%5D=step_sources"
    
    metadata_url <- paste0(base_api_url, "v2/users/", context$auth_data$user_id, "?", fields_part)
    result <- get_request_for_url(metadata_url, context=context, hdrs=c(Accept = "application/json"))
    httr::stop_for_status(result)

    httr::content(result,  "parsed")
}

get_request_for_url <- function(url, context = NA, hdrs=character(length = 0), ...) {
    if (!is.na(context[1])) {
        hdrs <- append(hdrs, c(
                                Authorization = paste("Bearer", context$auth_data$access_token),
                                "mfp-client-id" = "mfp-main-js",
                                "mfp-user-id" = context$auth_data$user_id))
    }
    hdrs <- append(hdrs, c(
                            "user-agent" = "Mozilla/5.0",
                            "Cache-Control" = "no-cache"))
    httr::GET(url, httr::add_headers(.headers=hdrs))
}

get_document_for_url <- function(url, context) {
    response <- get_request_for_url(url, context = context)
    httr::stop_for_status(response)
    httr::content(response, "parsed")
}

get_url_for_measurements <- function(page = 1, measurement_id = 1) {
    paste0(base_url, "measurements/edit", "?page=", page, "&type=", measurement_id)
}

get_measurement_ids <- function(document)
{
    options <- xml2::xml_find_all(document, "//select[@id='type']/option")
    os <- sapply(options, function(o) xml2::xml_attr(o, "value"))
    names(os) <- sapply(options, function(o) xml2::xml_text(o))
    os
}

get_numeric <- function(str) as.numeric(sub("[^0-9.]+", "", str))

get_table_from_document <- function(document) {
    # find the tr element for each measurement entry on the page
    trs <- xml2::xml_find_all(document, "//table[contains(@class,'check-in')]/tbody/tr")
    if (any(grepl("No measurements found.", xml2::xml_text(trs)))) {
        c()
    } else {
        trs
    }
}

get_measurements_from_table <- function(table)
    sapply(table, function(tr) get_numeric(xml2::xml_text(xml2::xml_children(tr)[3])))

get_dates_from_table <- function(table)
    do.call(c, lapply(table, function(tr) lubridate::mdy(xml2::xml_text(xml2::xml_children(tr)[2]))))

#' Retrieves measurements of a specific type from myfitnesspal within a given time interval.
#'
#' Retrieves MFP measurements of the given measurement type within the given date interval using
#' a series of HTTP requests and scraping the measurement from each table.
#' @title Retrieve MFP measurements
#' @param context MFP context object, containing authentication information.
#' @param measurement Type of measurement to retrieve, for example "Weight" or "Waist"
#' @param upper_bound Upper bound of measurements as a POSIXct object
#' @param lower_bound Lower bound of measurements as a POSIXct object
#' @return Dataframe containing two vectors: "dates" and "measurements"
#' @export
#' @author Edwin De Jong
mfp_measurements <- function(
                     context,
                     measurement="Weight",
                     upper_bound=lubridate::now(),
                     lower_bound=upper_bound - lubridate::days(30))
{
    # get the URL for the main check in page
    document <- get_document_for_url(get_url_for_measurements(), context = context)

    # gather the IDs for all measurement types
    measurement_ids <- get_measurement_ids(document)
    measurement_id <- measurement_ids[measurement]

    if (is.na(measurement_id))
        stop(paste("Measurement", measurement, "does not exist."))

    page <- 1
    dates <- c()
    measurements <- c()

    repeat {
        document <- get_document_for_url(
            get_url_for_measurements(page = page, measurement_id = measurement_id),
            context = context)
        table <- get_table_from_document(document)

        if (length(table) == 0) break

        dates <- append(dates, get_dates_from_table(table))
        measurements <- append(measurements, get_measurements_from_table(table))

        if (min(dates) < lower_bound) break
        
        page <- page + 1
    }

    df <- data.frame(measurements, dates)
    df[df$dates <= upper_bound & df$dates >= lower_bound,]
}

get_url_for_diary = function(
              upper_bound = lubridate::now(),
              lower_bound = upper_bound - lubridate::days(300)) {
    from <- format(lower_bound, "%Y-%m-%d")
    to <- format(upper_bound, "%Y-%m-%d")

    paste0(base_url, "reports/printable_diary", "?from=", from, "&to=", to)
}

getNthMeasurement <- function(node, idx) {
    el <- xml2::xml_find_first(node, paste0("tfoot/tr/td[", idx, "]/text()"))
    if (length(el) == 0) {
        return(NA)
    } else {
        ret <- get_numeric(xml2::xml_text(el))
        if (ret == 0) return(NA) else return(ret)
    }
}

##' Retrieves a MFP diary of foods and exercises.
##'
##' With this function and an active context, a diary within a certain time-frame can
##' be downloaded and inserted into a dataframe. The data-frame contains rows with the
##' date, exercise information and food information. 
##'
##' @title Retrieve MFP diary
##' @param context The context object (can be retrieved using mfp_context)
##' @param upper_bound The upper bound of the diary retrieved
##' @param lower_bound The lower bound of the diary retrieved
##' @return A data.frame with the diary entries.
##' @author Edwin De Jong
mfp_diary = function(
              context,
              upper_bound = lubridate::now(),
              lower_bound = upper_bound - lubridate::days(300)) {

    document <- get_document_for_url(
        get_url_for_diary(lower_bound = lower_bound, upper_bound = upper_bound),
        context = context)

    # Find all elements which contain useful data
    content_elements <- xml2::xml_find_all(
        document,
        "//h2[@class='main-title-2'] | //table[@class='table0']")

    if (length(content_elements) == 0) {
        stop("Could not find content element")
    }

    # Number of dates found
    n <- length(xml2::xml_find_all(document, "//h2[@class='main-title-2']")) - 1

    df <- data.frame(
        date = as.POSIXct(integer(n), origin=lubridate::now()),
        caloriesBurned = numeric(n) * NA,
        minutes = numeric(n) * NA,
        sets = numeric(n) * NA,
        weights = numeric(n) * NA,
        caloriesTaken = numeric(n) * NA,
        carbs = numeric(n) * NA,
        fat = numeric(n) * NA,
        protein = numeric(n) * NA,
        cholesterol = numeric(n) * NA,
        sodium = numeric(n) * NA,
        sugars = numeric(n) * NA,
        fibers = numeric(n) * NA
    )

    i <- -1 
    for (node in content_elements) {
        if (xml2::xml_attr(node, "id") == "date") {
            # Found a date header
            i <- i + 1
            df$date[i] <- lubridate::parse_date_time(xml2::xml_text(node), orders=c("B d, Y!"))
        } else {
            table_type <- xml2::xml_text(xml2::xml_find_first(node, "thead/tr/td[1]/text()"))
            if (table_type == "Exercises") {
                # Found an exercises table
                df$caloriesBurned[i] <- getNthMeasurement(node, 2)
                df$minutes[i] <- getNthMeasurement(node, 3)
                df$sets[i] <- getNthMeasurement(node, 4)
                df$weights[i] <- getNthMeasurement(node, 5)
            }
            if (table_type == "Foods") {
                # Found an foods table
                df$caloriesTaken[i] <- getNthMeasurement(node, 2)
                df$carbs[i] <- getNthMeasurement(node, 3)
                df$fat[i] <- getNthMeasurement(node, 4)
                df$protein[i] <- getNthMeasurement(node, 5)
                df$cholesterol[i] <- getNthMeasurement(node, 6)
                df$sodium[i] <- getNthMeasurement(node, 7)
                df$sugars[i] <- getNthMeasurement(node, 8)
                df$fibers[i] <- getNthMeasurement(node, 9)
            }
        }
    }
    # Remove any empty columns (columns containing only NA)
    df[, colSums(is.na(df)) != nrow(df)]
}
