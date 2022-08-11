ridgelines <- function(x, grp, xlim, ylim, xlab, adjust=1, n=TRUE, height=0.8, col, ...) {

   # check if grp is a factor; if not, turn it into one

   if (!is.factor(grp))
      grp <- factor(grp)

   # get the groups and the number of groups

   grps <- levels(grp)
   p    <- nlevels(grp)

   # if col is not specified, use (semi-transparent) colors from the rainbox
   # palette; if it is specified and it is a single value, repeat it p times;
   # then check if col is of the same length as p

   if (missing(col)) {
      col <- rainbow(p, alpha=0.2)
   } else {
      if (length(col) == 1L)
         col <- rep(col, p)
   }

   if (length(col) != p)
      stop("Length of 'col' does not match number of groups (", p, ").")

   if (missing(xlim))
      xlim <- range(x, na.rm=TRUE)

   if (missing(ylim))
      ylim <- c(0.8, p+height)

   if (missing(xlab))
      xlab <- ""

   plot(NA, xlim=xlim, ylim=ylim, xlab=xlab, ylab="", yaxt="n", bty="n", ...)
   axis(side=2, at=1:p, labels=grps, las=1, tick=FALSE, ...)

   for (i in p:1) {

      abline(h = i, col="lightgray")
      res <- density(x[grp == grps[i]], na.rm=TRUE, adjust=adjust)
      res$y <- res$y / max(res$y) * height
      lines(res$x, res$y + i, ...)
      polygon(res$x, res$y + i, col=col[i], ...)
      if (n)
         text(xlim[2] + (xlim[2]-xlim[1])*.04, i + .15, paste("n =", length(x[grp == grps[i]])), pos=2, cex=0.8)

   }

}
