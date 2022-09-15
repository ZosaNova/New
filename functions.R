
## returns the xts object

loadSeries <- function(sym, data.source, startDate, endDate) {
    xts.ohlc <- getSymbols(Symbols=sym, src=data.source,
                           from=startDate, to=endDate, auto.assign=F, adjust=T)
    sym.clean <- gsub("\\^", "", sym) # remove index prefix characters from sym

    xts.r <- merge(dailyReturn(xts.ohlc), monthlyReturn(xts.ohlc))
    
    colnames(xts.r) <- c(paste(sym.clean, ".", "Return", sep=""), 
                         paste(sym.clean, ".", "Return.Monthly", sep=""))
    merge(xts.ohlc, xts.r)
}

## returns an xts object containing the modified price data

cleanSeries <- function(xts.prices, sym.old, sym.new, refValue) {
    names(xts.prices) <- sub(paste("^", sym.old, sep=""),
                             paste(sym.new), 
                             names(xts.prices))

    if(!is.na(refValue)) {
        index <- grep(sym.new, names(xts.prices))
        target.series <- xts.prices[1, index]

        factor <- refValue / target.series
        xts.prices[, index] <- xts.prices[, index] * rep(factor)
    }
    xts.prices
}

## from an xts obj, fetch the price from the named column at the specified date
priceAtDate <- function(xts.prices, col.name, date) {
    col.index <- grep(col.name, names(xts.prices))
    as.vector(xts.prices[index(xts.prices) == date, col.index])[1]
}

## returns a list containing the etf price data in xts objects

loadETF <-function(xts.base, source, symbol.etf, type, prefix) {
    ## load the ETF and transform the data
    xts.etf <- loadSeries(symbol.etf, source, startDate, endDate)
    start.date <- index(xts.etf)[1]
    start.val <- priceAtDate(xts.base, type, start.date)
    row.name <- paste(symbol.etf, ".", type, sep="")
    row.name.new <- paste(prefix, ".", row.name, sep="")
    xts.etf <- cleanSeries(xts.etf, row.name, row.name.new, start.val)

    list("etf"=xts.etf)
}

## returns the ggplot object

xtsMultiPlot <- function(xts.merged, symbol.base, colors, title) {
    ## convert to dataframe 
    df.merged <- as.data.frame(xts.merged)
    ## create separate data column
    df.merged$Date <- as.Date(rownames(df.merged))
    ## melt by date
    df.melted <- melt(df.merged, id.vars=c("Date"))
    colnames(df.melted) <- c("Date", "Price", "Value")
    ## decompose all columns containing closing prices into rows
    df.filtered <- df.melted[grep("Close", df.melted$Price), ]
    ## render the plot of prices vs. date for each variable series
    ggplot(df.filtered, aes(x=Date, y=Value, color=Price)) + geom_line() +
        scale_color_manual(values=colors) + 
        ylab("Price") + ggtitle(paste(symbol.base, "vs.", title))
}