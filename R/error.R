error <- function (..., call. = TRUE, domain = NULL, class = NULL, call = sys.call(-1L))
stop(errorCondition(message = .makeMessage(..., domain = domain),
        class = class, call = if (call.) call))
