baseConvert <- function(x, target, base=10) {
    ifelse(is.na(x),
           x,
           {
    olddim <- dim(x)
    # Value -> Digit
    characters <- c(seq(0,9), LETTERS)
    # Digit -> Value
    numbers <- structure(seq(0,35), names=characters)
    if (is.numeric(x)) {
        x <- abs(x)
    } else if (is.character(x)) {
        x <- toupper(x)
    }
    if (base > 10 && !is.character(x)) {
        stop("Parameter x must be of mode character for bases greater than 10.")
    }
    if (base < 2 || base > 36) {
        stop("Base of x must be [2,36]")
    }
    if (target < 2 || target > 36) {
        stop("Target base for x must be [2,36]")
    }
    if (base != 10) {
        x <- strsplit(as.character(x), "")
        if (any(!unlist(x) %in% characters[seq(base)]))
            stop("Invalid number for base.")
    }

    # Convert to base 10
    if (base != 10) {
        l <- lapply(x, length)
        f1 <- function(x, l) {
            sum(numbers[x] * base ^ (seq(l - 1,0)))
        }
        sum <- mapply(f1, x, l)
        names(sum) <- NULL
    } else {
        sum <- x
    }

    result <- c()
    # Convert to new base
    if (target != 10) {
        f2 <- function(sum) {
            if (sum > 0) {
                d <- floor(log(sum, target) + 1)
                paste(characters[abs(diff(sum %% target^seq(d,0))) %/% target^seq(d-1,0) + 1], collapse="")
            } else {
                '0'
            }
        }
        result <- mapply(f2, sum)
    } else {
        result <- sum
    }

    structure(result, dim=olddim)
})
}
