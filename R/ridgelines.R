ridgelines <- function(x, grp, xlim, ylim, xlab="", adjust=1, height=0.8,
                       fill, alpha=0.2,
                       col="black", abcol="lightgray",
                       yscale="maxall", # "pergrp"
                       bw=mean, # use I() to use the bw per group
                       verbose=TRUE,
                       n=FALSE, ...) {

   # match yscale to options (and check that it is of length 1)

   yscale <- match.arg(yscale, c("maxall", "pergrp"))

   # check if grp is a factor; if not, turn it into one

   if (!is.factor(grp))
      grp <- factor(grp)

   # get the groups and the number of groups

   grps <- levels(grp)
   p    <- nlevels(grp)

   # if fill is not specified, use (semi-transparent) colors from the rainbow
   # palette; if it is specified, recycle as often as needed to get p colors

   if (missing(fill)) {
      fill <- rainbow(p, alpha=alpha)
   } else {
      fill <- col2rgb(fill) / 255
      fill <- apply(fill, 2, function(x) rgb(x[1], x[2], x[3], alpha))
      fill <- rep_len(fill, p)
   }

   col <- rep_len(col, p)

   # set defaults for xlim and ylim

   if (missing(xlim))
      xlim <- range(x, na.rm=TRUE)

   if (missing(ylim))
      ylim <- c(0.8, p+height)

   ddd <- list(...)

   if (is.null(ddd$kernel)) {
      kernel <- "gaussian"
   } else {
      kernel <- ddd$kernel
   }

   lplot    <- function(..., kernel) graphics::plot(...)
   laxis    <- function(..., kernel) graphics::axis(...)
   llines   <- function(..., kernel) graphics::lines(...)
   lpolygon <- function(..., kernel) graphics::polygon(...)
   labline  <- function(..., kernel) graphics::abline(...)

   #########################################################################

   # get the kernel density estimate for each group and store them in the list 'res'

   res <- list()

   for (i in p:1) {
      res[[i]] <- density(x[grp == grps[i]], na.rm=TRUE)
   }

   # either 'bw' is a function or a (vector of) values
   # by default, bw=mean, so the mean bandwidth across all groups is used
   # could also set bw=I, to use the bandwidth values per group
   # if nw is numeric, then use this value (and recycle as often as necessary)

   if (is.function(bw)) {

      bwi <- sapply(res, function(x) x$bw)

      bw <- bw(bwi)

      if (length(bw) == 1L) {
         bwforcat <- bw
         bw <- rep(bw, p)
      } else {
         if (length(bw) != p)
            stop(paste0("Length of generated 'bw' values does not match number of groups (", p, ")."))
         bwforcat <- bw
      }

   } else {

      if (length(bw) == 1L) {
         bwforcat <- bw
         bw <- rep_len(bw, p)
      } else {
         bw <- rep_len(bw, p)
         bwforcat <- bw
      }

   }

   if (verbose) {

      if (anyNA(x)) {
         nna <- sum(is.na(x))
         message("Removed ", nna, " missing value", ifelse(nna > 1, "s", ""), ".")
      }

      bwforcat <- round(bwforcat, 2)

      if (length(bwforcat) == 1L) {
         message("Joint bandwidth value: ", bwforcat)
      } else {
         message("Bandwidth values: ", paste0(bwforcat, collapse=", "))
      }

   }

   # run the kernel density estimation again with the chosen bandwidth(s)

   for (i in p:1) {
      res[[i]] <- density(x[grp == grps[i]], na.rm=TRUE, bw=bw[i], adjust=adjust, kernel=kernel)
   }

   # get the maximum of y for each group

   grpmax <- sapply(res, function(x) max(x$y))

   # compute the rescaling factor depending on the chosen yscale

   if (yscale == "maxall") {
      rfactor <- rep(max(grpmax), p)
   } else {
      rfactor <- grpmax
   }

   #########################################################################

   # set up the empty plot

   lplot(NA, xlim=xlim, ylim=ylim, xlab=xlab, ylab="", yaxt="n", bty="n", ...)

   # add the y-axis labels (i.e., the group labels)

   laxis(side=2, at=1:p, labels=grps, las=1, tick=FALSE, ...)

   #grid()

   # plot the densities

   for (i in p:1) {

      xs <- res[[i]]$x
      ys <- res[[i]]$y

      labline(h = i, col=abcol, ...)

      ys <- ys / rfactor[i] * height
      llines(xs, ys + i, col=col[i], ...)
      lpolygon(xs, ys + i, col=fill[i], border=NA, ...)

      #if (n)
      #   text(xlim[2] + (xlim[2]-xlim[1])*.04, i + .15, paste("n =", length(x[grp == grps[i]])), pos=2, cex=0.8)

   }

}
