##----------------------------------------------------------------------
## Program: SAS.R
## Project: p:/PECARN/ASSESS/Statistical Analysis
## Directory: programs/Lead/Manuscripts/Mental health symptoms/Macros
## Author(s): Tim Simmons
## Purpose: Defines a closure that exposes function to define and query
##   SAS libraries and apply user formats to variables
##
## Start Date: 2016-11-17
##----------------------------------------------------------------------


SAS.System <- function() {
    e <- new.env()

    date.fmts <- c(
      "B8601DA."
      , "DATE."
      , "DAY."
      , "DDMMYY."
      , "DDMMYYD."
      , "DOWNAME."
      , "DTDATE."
      , "E8601DA."
      , "JULDAY."
      , "JULIAN."
      , "MMDDYY."
      , "MMDDYYD."
      , "MMYY."
      , "MMYYD."
      , "MONNAME."
      , "MONTH."
      , "MONYY."
      , "PDJULG."
      , "PDJULI."
      , "QTR."
      , "QTRR."
      , "WEEKDATE."
      , "WEEKDAY."
      , "WORDDATE."
      , "YEAR."
      , "YYMM."
      , "YYMMDD."
      , "YYMMDDD."
      , "YYMON."
      , "YYQ."
      , "YYQD."
      , "YYQR."
      , "YYQRD."
    )

    datetime.fmts <- c(
      "B8601DN."
      , "B8601DT."
      , "B8601DZ."
      , "B8601LX."
      , "DATEAMPM."
      , "DATETIME."
      , "E8601DN."
      , "E8601DT."
      , "E8601DZ."
      , "E8601LX."
      , "MDYAMPM."
      , "TIMEAMPM."
      , "TOD."
      , "B8601DX."
      , "DTMONYY."
      , "DTWKDATX."
      , "DTYEAR."
      , "DTYYQC."
      , "E8601DX."
    )

    read.sas.tables <- function(path) {
        memname <- list.files(path, pattern="*.sas7bdat")
        memname <- toupper(gsub("[.]sas7bdat$", "", memname))
        memname
    }

    read.sas.contents.from.table <- function(tbl) {
        ntbl <- names(tbl)
        type <- sapply(ntbl, function(x) mode(tbl[[x]]))

        format <- sapply(ntbl, function(x) {
            attrib <- attributes(tbl[[x]])
            if ("format.sas" %in% names(attrib))
                paste0(toupper(attrib[["format.sas"]]), ".")
            else
                NA
        })

        label <- sapply(ntbl, function(x) {
            attrib <- attributes(tbl[[x]])
            if ("label" %in% names(attrib))
                attrib[["label"]]
            else
                NA
        })

        contents <- data.frame(
            name=ntbl,
            type=type,
            format=format,
            label=label, stringsAsFactors=FALSE)
        row.names(contents) <- NULL
        contents
    }

    read.sas.contents <- function(path, member) {
        tables <- read.sas.tables(path)
        stopifnot(toupper(member) %in% tables)

        tbl <- haven::read_sas(paste0(path, .Platform$file.sep, tolower(member), ".sas7bdat"))
        contents <- read.sas.contents.from.table(tbl)
        contents
    }

    read.sas <- function(path, member, format=TRUE) {
        tables <- read.sas.tables(path)
        stopifnot(toupper(member) %in% tables)

        tbl <- haven::read_sas(paste0(path, .Platform$file.sep, tolower(member), ".sas7bdat"))


        contents <- read.sas.contents.from.table(tbl)

        tbl <- do.call(
            function(...) data.frame(..., stringsAsFactors=FALSE),
            lapply(tbl, function(x) {
                attributes(x) <- NULL
                x
            }))

        if (is.null(e$formats) || !format)
          return(tbl)

        for (column in names(tbl)) {
            column.format <- contents[contents$name == column, "format"]
            column.type <- contents[contents$name == column, "type"]


            if (column.format %in% e$formats$FMTNAME) {

                catalog <- e$formats[e$formats$FMTNAME == column.format, c("FMTNAME", "START", "LABEL")]

                catalog$START <-
                    if (column.type == "numeric")
                        suppressWarnings(as.numeric(catalog$START))
                    else
                        catalog$START

                catalog <- na.omit(catalog)
                if (anyDuplicated(catalog$LABEL)) {
                  dup <- duplicated(catalog$LABEL)
                  catalog$LABEL[dup] <- paste0(catalog$LABEL[dup], " - ", catalog$START[dup])
                }

                tbl[[column]] <- factor(tbl[[column]],
                                        levels=catalog$START,
                                        labels=catalog$LABEL)
            } else if (column.format %in% date.fmts) {
                tbl[[column]] <- as.Date(tbl[[column]], origin="1970-01-01")
            } else if (column.format %in% datetime.fmts) {
                tbl[[column]] <- as.POSIXct(tbl[[column]], origin="1970-01-01", tz="UTC")
            }
        }
        return(tbl)
    }


    libname <- function(libref, path) {

        tables <- read.sas.tables(path)
        if ("FORMATS" %in% tables) {

            formats <- read.sas(path, "formats", format=FALSE)
            formats$FMTNAME <- paste0(formats$FMTNAME, ".")

            if (!is.null(e$formats)) {
                new.formats <- !(formats$FMTNAME %in% e$formats$FMTNAME)
                formats <- rbind(e$formats, formats[new.formats, ])
            }
            assign("formats", formats, e)
        }

        assign(libref,
               list(
                 data=function(tbl) read.sas(path, tbl),
                 contents=function(tbl) read.sas.contents(path, tbl),
                 tables=function() read.sas.tables(path),
                 path=path)
               , e)
    }

    assign("libname", libname, e)
    e
}
