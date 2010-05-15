PlotBreakPoints <-
function(x, Chromosome, layout=c(length(unique(Chromosome)),1), ...)
{
    #
    # Trellis plot for getting break points separtely for each chromosome: 
    #
    # This function will plot the means for different breaks. We can specify whether to smooth 
    # the data before the analysis. If we have only one chromosome number or we want to analyze 
    # an array without specific chromosome, then simply provide "Chromosome= rep(1, length(x))"
    # to get the result. 
    #
    # x           : Array-CGH data
    # Chromosome  : Sequence of chromosomes, eg. 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3 and so on.
    # layout      : Layout of the lattice plot
    # ...         : Arguments that can be passed to plot command

    ch.n <- table(Chromosome)
    GeneLabel <- unlist(apply(ch.n, 1, function(x) list(1:x)))
    Ch <- ordered(Chromosome)
    numCh <- length(unique(Chromosome))
    g.df <- data.frame(xr = x, GeneLabel = GeneLabel, Ch = Ch)
    xyplot(xr ~ GeneLabel | Ch, layout = layout, strip = function(...)
    strip.default(..., style = 5), 
    panel = function(x, y)
    {
       panel.xyplot(x, y, pch = 19, ...)
       yB <- BreakPoints(y)
       panel.abline(v=yB, col="red", lwd=2)
        }
    , data = g.df, xlab = "Clone", ylab = "Values")
   }

