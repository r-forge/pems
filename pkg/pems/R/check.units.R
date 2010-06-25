check.units <- function (input, input.name, input.class, fun.name, input.units = NULL, 
    fun.args = list("convert", "default"), source, pems) 
{
    rep.1 <- paste(" ", fun.name, " units conflict", sep = "")
    if (is.null(input.units)) {
        if (is.null(comment(input))) {
            input.units <- pems$units[1, input.name]
        }
        else {
            if (length(grep("units:", comment(input))) > 0) {
                input.units <- grep("units:", comment(input), 
                  value = TRUE)[1]
                input.units <- gsub("units:", "", input.units)
                if (input.units == "") {
                  input.units <- NULL
                }
            }
        }
    }
    if (is.null(input.units)) {
        rep.2 <- paste("no units assigned to ", input.class, 
            " input '", input.name, "'", sep = "")
        stop(paste(rep.1, rep.2, sep = "\n\t"), call. = FALSE, 
            domain = NA)
    }
    input.units <- as.character(input.units)
    this.correction <- 0
    if (input.class == "speed") {
        def.units <- "m/s"
        if (input.units == "m/s") {
            this.correction = 1
        }
        if (input.units == "km/h") {
            this.correction = 0.27777777778
        }
    }
    if (input.class == "accel") {
        def.units <- "m/s/s"
        if (input.units == "m/s/s") {
            this.correction = 1
        }
    }
    if (input.class == "time") {
        def.units <- "s"
        if (input.units == "s") {
            this.correction = 1
        }
    }
    if (input.class == "em") {
        def.units <- "g/s"
        if (input.units == "g/s" | input.units == "g/sec") {
            this.correction = 1
        }
    }
    if (this.correction == 0) {
        rep.2 <- paste("assigned units of ", input.class, " input '", 
            input.name, "' [", input.units, "] not recognised", 
            sep = "")
        stop(paste(rep.1, rep.2, sep = "\n\t"), call. = FALSE, 
            domain = NA)
    }
    input <- input * this.correction
    comment(input) <- c(paste("units:", def.units, sep = ""), 
        comment(input))
    input
}
