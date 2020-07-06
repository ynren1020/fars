#' read in file function
#'
#' This is a function to read in data file in csv format and its output
#' is a data frame tbl.
#'
#' @param filename A string of characters shows the name of data file
#'
#' @return The returned file is a data frame tbl.
#'
#' @details If the file does not exist, it will give an error message.
#'
#' @import  readr
#' @import  dplyr
#'
#' @example
#' fars_read("./data/accident_2013.csv")
#'
#' @export
fars_read <- function(filename) {
    if(!file.exists(filename))
        stop("file '", filename, "' does not exist")
    data <- suppressMessages({
        readr::read_csv(filename, progress = FALSE)
    })
    dplyr::tbl_df(data)
}

#' create file name by year
#'
#' This is a function to create a file name for accident by using year,
#' which can be customized by users.
#'
#' @param year  the year, a numeric value
#'
#' @return A character string contains year information as file name
#'
#' @example
#' make_filename(2019)
#'
#' @export
make_filename <- function(year) {
    year <- as.integer(year)
    sprintf("accident_%d.csv.bz2", year)
}

#' Read a list of years' accident files
#'
#' Read in a list of different years' accident files, add year info as a column
#' for each data frame, and choose MONTH and year columns for futher analysis,
#' if the input of year is invalid, it will give a message and return no result.
#'
#' @param years A list of numeric values standing for years
#'
#' @details If years is not invalid number for years, a warning will be issused.
#'
#' @return A list of data frames with two columns in each of them, MONTH and year
#'
#' @example
#' fars_read_years(list(2019, 2018, 2017, 2016))
#'
#' @export
fars_read_years <- function(years) {
    lapply(years, function(year) {
        file <- make_filename(year)
        tryCatch({
            dat <- fars_read(file)
            dplyr::mutate(dat, year = year) %>%
                dplyr::select(MONTH, year)
        }, error = function(e) {
            warning("invalid year: ", year)
            return(NULL)
        })
    })
}

#' Sum of accidents by year and month
#'
#' Take in a list of years and summarize how many accidents happened in each month
#' each year, returns a data frame with years as columns, rows as months, each cell
#' is the number of accident happened in that month (row) of that year (column).
#'
#' @param years A list of numeric values which are years
#'
#' @return A data frame with counts of accidents for each month each year
#'
#' @import dplyr
#' @import tidyr
#'
#' @example
#' fars_summarize_years(list(2020, 2019,2018, 2017))
#'
#' @export
fars_summarize_years <- function(years) {
    dat_list <- fars_read_years(years)
    dplyr::bind_rows(dat_list) %>%
        dplyr::group_by(year, MONTH) %>%
        dplyr::summarize(n = n()) %>%
        tidyr::spread(year, n)
}

#' Plot accidents by state and year
#'
#' plot accidents by lattitude and longitude for state and year inputed by users.
#'
#' @param state.num the numbers stand for state
#' @param year a numeric value
#'
#' @return A map plot for input state and year will be produced.
#'
#' @import maps
#' @import graphics
#'
#' @details the input state.num should be in the STATE column of data, otherwise
#'          error message "invalid STATE number" will be given; If there is no accident
#'          for the input state.number and year, a message will state "no accidents to plot".
#'
#' @example
#' fars_map_state(1,2010)
#'
#' @export
fars_map_state <- function(state.num, year) {
    filename <- make_filename(year)
    data <- fars_read(filename)
    state.num <- as.integer(state.num)

    if(!(state.num %in% unique(data$STATE)))
        stop("invalid STATE number: ", state.num)
    data.sub <- dplyr::filter(data, STATE == state.num)
    if(nrow(data.sub) == 0L) {
        message("no accidents to plot")
        return(invisible(NULL))
    }
    is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
    is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
    with(data.sub, {
        maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                  xlim = range(LONGITUD, na.rm = TRUE))
        graphics::points(LONGITUD, LATITUDE, pch = 46)
    })
}
