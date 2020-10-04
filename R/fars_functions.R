#' Read FARS Data
#'
#' The function load data from the US National Highway Traffic Safety Administration's Fatality
#' Analysis Reporting System (FARS):
#' \url{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}
#'
#' @param filename The name of the file to be read to tbl_df object
#'
#' @return Tibble with data from the supplied file.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#'    \dontrun{fars_read("accident_2014.csv.bz2")}
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

#' Construct filename with provided year
#'
#' Function expects \code{year} as input and
#' constructs the FARS filename for the given year.
#'
#' @param year A year provided as 4 digit integer or string
#'
#' @return FARS filename for given year
#'
#' @examples
#'    \dontrun{make_filename(2014)}
#'    \dontrun{make_filename("2014")}
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read multiple years of FARS Data
#'
#' Funtion takes vector of years and converted each year to FARS filename.
#' Each data frame contains just the year and month. If FARS data file
#' is not available for any year, the list entry for that year is NULL
#'
#' @param years Vector of years denoted as 4 digit integers or strings
#'
#' @return Function returns a list of data frames with FARS data
#'
#' @importFrom dplyr mutate select
#'
#' @examples
#'    \dontrun{fars_read_years(c(2013,2015))}
#'    \dontrun{fars_read_years(2013:2015)}
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
      error("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Summarize multiple years FARS Data
#'
#' Multiple years FARS data is summarized
#' by counting the number of rows for each year and month.
#' The summarized data frame displays months as rows and years as columns.
#'
#' @param years Vector of years denoted as 4 digit integers or strings
#'
#' @return Returns data frame with FARS data summarized by years and months.
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom magrittr %>%
#' @importFrom tidyr spread
#'
#' @examples
#'    \dontrun{fars_summarize_years(c(2013,2015))}
#'    \dontrun{fars_summarize_years((2013:2015))}
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = dplyr::n()) %>%
    tidyr::spread(year, n)
}

#' Plot FARS Data By State
#'
#' Function filter data for given state and use it to plot a heat map where area with more accidents
#' will be shown in darker spots. The function will print a message "no accident to plot"
#' if there is no accident to report.
#'
#' @param state.num An integer corresponding to the state number. Valid values are 1 to 51, except 3
#' @param year A year provided as 4 digit integer or string
#'
#' @return This function plots a heat map of the accident for the given state and year.
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @note Though the function anticipates giving a message if no accident is reported
#' for a state in a given year, there is a very good chance that this portion of the
#' code will not be trigerred due to a check to ensure the \code{state.num} filter
#' parameter is in the data set.
#'
#' @examples
#'    \dontrun{fars_map_state(1,2015)}
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
