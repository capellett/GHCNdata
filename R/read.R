# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

## Custom function takes GHCND station code as input,
## Returns long data.frame of observations as output
## Might want to spread by Element...
read_ghcndly <- function(station_code) {
  StationFile <- paste0("C:/Users/pellettc/Documents/GIS/Data/GHCND/ghcnd_all/", station_code, ".dly")
  fixed_widths <- c(11, 4, 2, 4, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1, 5, 1, 1, 1)
  column_names <- c('ID', 	'YEAR', 	'MONTH', 	'ELEMENT', 	'VALUE1', 	'MFLAG1', 	'QFLAG1', 	'SFLAG1', 	'VALUE2', 	'MFLAG2', 	'QFLAG2', 	'SFLAG2', 	'VALUE3', 	'MFLAG3', 	'QFLAG3', 	'SFLAG3', 	'VALUE4', 	'MFLAG4', 	'QFLAG4', 	'SFLAG4', 	'VALUE5', 	'MFLAG5', 	'QFLAG5', 	'SFLAG5', 	'VALUE6', 	'MFLAG6', 	'QFLAG6', 	'SFLAG6', 	'VALUE7', 	'MFLAG7', 	'QFLAG7', 	'SFLAG7', 	'VALUE8', 	'MFLAG8', 	'QFLAG8', 	'SFLAG8', 	'VALUE9', 	'MFLAG9', 	'QFLAG9', 	'SFLAG9', 	'VALUE10', 	'MFLAG10', 	'QFLAG10', 	'SFLAG10', 	'VALUE11', 	'MFLAG11', 	'QFLAG11', 	'SFLAG11', 	'VALUE12', 	'MFLAG12', 	'QFLAG12', 	'SFLAG12', 	'VALUE13', 	'MFLAG13', 	'QFLAG13', 	'SFLAG13', 	'VALUE14', 	'MFLAG14', 	'QFLAG14', 	'SFLAG14', 	'VALUE15', 	'MFLAG15', 	'QFLAG15', 	'SFLAG15', 	'VALUE16', 	'MFLAG16', 	'QFLAG16', 	'SFLAG16', 	'VALUE17', 	'MFLAG17', 	'QFLAG17', 	'SFLAG17', 	'VALUE18', 	'MFLAG18', 	'QFLAG18', 	'SFLAG18', 	'VALUE19', 	'MFLAG19', 	'QFLAG19', 	'SFLAG19', 	'VALUE20', 	'MFLAG20', 	'QFLAG20', 	'SFLAG20', 	'VALUE21', 	'MFLAG21', 	'QFLAG21', 	'SFLAG21', 	'VALUE22', 	'MFLAG22', 	'QFLAG22', 	'SFLAG22', 	'VALUE23', 	'MFLAG23', 	'QFLAG23', 	'SFLAG23', 	'VALUE24', 	'MFLAG24', 	'QFLAG24', 	'SFLAG24', 	'VALUE25', 	'MFLAG25', 	'QFLAG25', 	'SFLAG25', 	'VALUE26', 	'MFLAG26', 	'QFLAG26', 	'SFLAG26', 	'VALUE27', 	'MFLAG27', 	'QFLAG27', 	'SFLAG27', 	'VALUE28', 	'MFLAG28', 	'QFLAG28', 	'SFLAG28', 	'VALUE29', 	'MFLAG29', 	'QFLAG29', 	'SFLAG29', 	'VALUE30', 	'MFLAG30', 	'QFLAG30', 	'SFLAG30', 	'VALUE31', 	'MFLAG31', 	'QFLAG31', 	'SFLAG31')
  value_columns <- c('VALUE1', 	'VALUE10', 	'VALUE11', 	'VALUE12', 	'VALUE13', 	'VALUE14', 	'VALUE15', 	'VALUE16', 	'VALUE17', 	'VALUE18', 	'VALUE19', 	'VALUE2', 	'VALUE20', 	'VALUE21', 	'VALUE22', 	'VALUE23', 	'VALUE24', 	'VALUE25', 	'VALUE26', 	'VALUE27', 	'VALUE28', 	'VALUE29', 	'VALUE3', 	'VALUE30', 	'VALUE31', 	'VALUE4', 	'VALUE5', 	'VALUE6', 	'VALUE7', 	'VALUE8', 	'VALUE9')
  Mflag_columns <- c('MFLAG1', 	'MFLAG10', 	'MFLAG11', 	'MFLAG12', 	'MFLAG13', 	'MFLAG14', 	'MFLAG15', 	'MFLAG16', 	'MFLAG17', 	'MFLAG18', 	'MFLAG19', 	'MFLAG2', 	'MFLAG20', 	'MFLAG21', 	'MFLAG22', 	'MFLAG23', 	'MFLAG24', 	'MFLAG25', 	'MFLAG26', 	'MFLAG27', 	'MFLAG28', 	'MFLAG29', 	'MFLAG3', 	'MFLAG30', 	'MFLAG31', 	'MFLAG4', 	'MFLAG5', 	'MFLAG6', 	'MFLAG7', 	'MFLAG8', 	'MFLAG9')
  Qflag_columns <- c('QFLAG1', 	'QFLAG10', 	'QFLAG11', 	'QFLAG12', 	'QFLAG13', 	'QFLAG14', 	'QFLAG15', 	'QFLAG16', 	'QFLAG17', 	'QFLAG18', 	'QFLAG19', 	'QFLAG2', 	'QFLAG20', 	'QFLAG21', 	'QFLAG22', 	'QFLAG23', 	'QFLAG24', 	'QFLAG25', 	'QFLAG26', 	'QFLAG27', 	'QFLAG28', 	'QFLAG29', 	'QFLAG3', 	'QFLAG30', 	'QFLAG31', 	'QFLAG4', 	'QFLAG5', 	'QFLAG6', 	'QFLAG7', 	'QFLAG8', 	'QFLAG9')
  Sflag_columns <- c('SFLAG1', 	'SFLAG10', 	'SFLAG11', 	'SFLAG12', 	'SFLAG13', 	'SFLAG14', 	'SFLAG15', 	'SFLAG16', 	'SFLAG17', 	'SFLAG18', 	'SFLAG19', 	'SFLAG2', 	'SFLAG20', 	'SFLAG21', 	'SFLAG22', 	'SFLAG23', 	'SFLAG24', 	'SFLAG25', 	'SFLAG26', 	'SFLAG27', 	'SFLAG28', 	'SFLAG29', 	'SFLAG3', 	'SFLAG30', 	'SFLAG31', 	'SFLAG4', 	'SFLAG5', 	'SFLAG6', 	'SFLAG7', 	'SFLAG8', 	'SFLAG9')
  varying_columns <- c(value_columns, Mflag_columns, Qflag_columns, Sflag_columns)

  raw <- read.fwf(StationFile, fixed_widths, header = FALSE, col.names = column_names, check.names = F, na.strings = "-9999")

  ## melt four times to keep the flags
  rawValues <- melt(raw, id = c(1:4), measure.vars = value_columns,
                    variable.name = "day")
  rawValues$day <- substring(rawValues$day, 6)

  rawMflags <- melt(raw, id = c(1:4), measure.vars = Mflag_columns,
                    variable.name = "day", value.name = "Mflag")
  rawMflags$day <- substring(rawMflags$day, 6)

  rawQflags <- melt(raw, id = c(1:4), measure.vars = Qflag_columns,
                    variable.name = "day", value.name = "Qflag")
  rawQflags$day <- substring(rawQflags$day,6)

  rawSflags <- melt(raw, id = c(1:4), measure.vars = Sflag_columns,
                    variable.name = "day", value.name = "Sflag")
  rawSflags$day <- substring(rawSflags$day,6)

  return(
    merge(
      merge(
        merge(rawValues, rawMflags, all = TRUE),
        rawQflags, all = TRUE),
      rawSflags, all = TRUE))
}
