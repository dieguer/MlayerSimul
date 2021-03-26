qqthin2 <- function (x, y, ends = c(0.01, 0.99), eps = 0.001, xlab = deparse(substitute(x)), 
    adj.xlab = NULL, ylab = deparse(substitute(y)), show.line = TRUE, 
    print.thinning.details = F, centerline = TRUE, ...) 
{
    xlab <- xlab
    ylab <- ylab
    x <- sort(x)
    y <- sort(y)
    dx <- diff(x)
    epsdist <- sqrt(diff(range(x))^2 + diff(range(y))^2) * eps
    dx <- 0.5 * (c(dx[1], dx) + c(dx, dx[length(dx)]))
    dy <- diff(y)
    dy <- 0.5 * (c(dy[1], dy) + c(dy, dy[length(dy)]))
    dpoints <- epsdist/sqrt(dx^2 + dy^2)
    dig <- floor(dpoints) + 1
    cdig <- round(cumsum(1/dig))
    subs <- match(unique(cdig), cdig)

    if (any(diff(subs) > 1)) {
        n1 <- min(subs[c(diff(subs), 0) > 1])
        n2 <- max(subs[c(0, diff(subs)) > 1])
        if (print.thinning.details) 
            print(paste("Graph retains", length(subs), "points."))

    }
    invisible(length(subs))
    return(as.data.frame(cbind(x[subs],y[subs])))
}