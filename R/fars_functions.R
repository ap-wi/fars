#' function "fars_read"
#'
#' @description
#'    Read a file that delivers data from the US National Highway Traffic Safety Administration's
#'    Fatality Analysis Reporting System and provide a data frame tbl
#'
#' @param filename  The name of the file which the data are to be read from.
#'    Each row of the table appears as one line of the file. If it does not
#'    contain an absolute path, the file name is relative to the current
#'    working directory, getwd(). Tilde-expansion is performed where supported.
#'    This can be a compressed file (see file).
#'
#' @return This function returns a tibble of ( "tbl_df", "tbl", "data.frame" )
#'
#' @details
#'    The function stopped with the message "file <filename> does not exist" if the file can not read.
#'
#' @references \url{https://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)}
#'
#' @examples
#' tf <- "accident_2015.csv.bz2"
#' fars_read( filename = tf )
#'
#' @export
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
fars_read <- function(filename) {
  if(!file.exists( filename )) {
    path <- path.expand( file.path( system.file(package = "fars") , "extdata" ) )
    filename <- file.path( path, filename )
  }
  if(!file.exists( filename )) {
    stop("file '", filename, "' does not exist") }
  data <- suppressMessages({
    readr::read_csv( filename, progress = FALSE )
  })
  dplyr::tbl_df(data)
}

#' function "make_filename"
#'
#' @description
#'    The function returns a filename for the given year
#'
#' @param year  A year (number with 4 digits)
#'
#' @return This function returns a filename as a character vector of length
#'    that of the longest input.
#'
#' @references \url{https://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)}
#'
#' @examples
#' yyyy <- 2015
#' make_filename <- function( year=yyyy )
#'
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' function "fars_read_years"
#'
#' @description
#'    The function returns a list of data frames per year which contains the corresponding
#'    columns "MONTH" and "year" with accidents
#'
#' @param years  A vector of years (a vector consisting of numbers with 4 digits)
#'
#' @return This function returns a list with data frame per year which contains the
#'    corresponding columns "MONTH" and "year" with accidents
#'
#' @details
#'    The function registered the warning "invalid year: <year>" and returns NULL
#'    if there is not a file for a year in the input parameter vector "years".
#'
#' @examples
#' yyyys <- c(2014, 2015)
#' fars_read_years( years = yyyys )
#'
#' @export
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#'
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

#' function "fars_summarize_years"
#'
#' @description
#'    The function returns a data frame with the number of accidents per month and year.
#'    The dimensions are the months per row and years per column.
#'
#' @param years  A vector of years (a vector consisting of numbers with 4 digits)
#'
#' @return This function returns a key-value pair across multiple columns
#'    (with rows per month and columns per year)
#'
#' @examples
#' yyyys <- c(2014, 2015)
#' fars_summarize_years( years=yyyys )
#'
#' @export
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom magrittr %>%
#' @importFrom tidyr spread
#'
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
    dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' function "fars_map_state"
#'
#' @description
#'    The function plot a map of a US state with the coordinates of the accidents in a year
#'
#' @param state.num A number of a US state (number with 2 digits)
#' @param year      A year (number with 4 digits)
#'
#' @return none
#'
#' @details
#'    The function stopped with the message invalid STATE number: ", <state.num>)"
#'    if no record with the state number in the data frame
#'
#'    The function generate the message "no accidents to plot" and returns the invisible value NULL
#'    if there is not any accident in the selected data
#'
#' @examples
#' stateno <- 1
#' yyyy <- 2015
#' fars_map_state( state.num=stateno, year=yyyy )
#'
#' @export
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
fars_map_state <- function( state.num, year ) {
  filename <- make_filename( year )
  data <- fars_read( filename )
  state.num <- as.integer( state.num )

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
