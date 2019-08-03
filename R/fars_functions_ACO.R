#' read a csv text file
#'
#' This function reads a csv format file and stores in dataframe tbl_df. For this task uses package "readr" which allows to directly read comressed files
#'
#' @param filename A character chain containing the name of the file you want to read
#'
#' @return This function returns a dataframe tbl_df
#'
#' @examples
#'
#' ## fars_read("accident_2013.csv.bz2")
#'
#' fars_read( system.file("extdata", "accident_2013.csv.bz2", package = "farsUSA") )
#'
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Creates a file name
#'
#' This function creates the apropriate name of the the file corresponding to accident data for the year 'year'
#'
#' @param year An integer indicating the year for filename creation
#'
#' @return This function returns a character variable containing a file name
#'
#' @examples
#' make_filename(2013)
#'
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Reads several csv text files
#'
#' This function reads several csv format files and stores month and year into a list of dataframes tbl_df,
#' containing month and year of the accidents stored, one tbl_df for each csv file.
#'
#' @param years A list containing the years to estract information
#'
#' @return This function returns a list of tbl_df from all years in "years" with month and year information
#'
#' @examples
#' fars_read_years(list(2013,2014))
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

#' Summarizes accidents by year and month
#'
#' This function summarizes the number of accidents occurred each month for a list of years
#'
#' @param years A list of years
#'
#' @return This function returns a tbl_df containing number of accidents by month
#' for the years included in the list parsed as parameter
#'
#' @examples
#' fars_summarize_years(list(2014,2015))
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Print a map of crash over year and state of U.S.
#'
#' This function prints a map with the position of every crash occurred over the state 'state.num' and the year 'year'
#'
#' @param state.num integer with the number of desired state, ranging 1:56
#' @param year An integert with the desired year
#'
#' @return This function returns a map
#'
#' @examples
#' fars_map_state(1, 2013)
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
