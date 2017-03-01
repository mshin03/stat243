# Private function : an auxiliary plotting function used by the main plot.toss
#   method.
#
# Description:
#   This function uses ggplot2::geom_point() to plot values related
#   to MFA, such as factor scores. It plots any 2 components of the MFA
#   as x- and y- axes.
#
# Arguments:
#   Dat: The data on which the MFA was performed. A matrix.
#   xdim, ydim: The two components the user wishes to plot. Numeric values.
#   mytitle:  The title of the plot. An optional parameter.
#   size:  The size of the points on the plot. Default is 3.
#   unit_length: The thickness of x-axis & y-axis. Default is 0.1
#   legend: An optional parameter that allows the user to control legend text.
#
myplot1 <- function(dat, xdim, ydim, mytitle = NA,
                    size = 3,unit_length = 0.1, legend){

  if ("plotID" %in% rownames(dat))
    stop("I'm afraid you cannot have a variable called 'plotID';
         This variable name is reserved by the plotting function.
         Please accept my humble apology for the inconvenience.")

  theme_set(theme_bw())

  myplot <- ggplot(dat, aes(x = dat[,xdim], y = dat[,ydim])) +

    geom_segment(aes(x = min(dat[,xdim]) - 0.5, y = 0, xend = max(dat[,xdim]) + 0.5, yend = 0),
                 arrow = arrow(length = unit(unit_length, "cm"))) +

    geom_segment(aes(x = 0, y = min(dat[,ydim]) - 0.5, xend = 0, yend = max(dat[,ydim]) + 0.5),
                 arrow = arrow(length = unit(unit_length, "cm"))) +

    # plot the position of each object as a point
    geom_point(aes(colour = plotID), size = size) +

    labs(x = paste0("x axis: comp ", as.character(xdim)),
         y = paste0("y axis: comp ", as.character(ydim))) +

    # Applying themes
    theme(
      axis.title = element_text(size=12, face="bold"),
      axis.text = element_text(size=0),
      axis.ticks = element_line(size=0),
      plot.title = element_text(size=15, face="bold", hjust=0.5),
      panel.grid.major = element_line(size=0),
      panel.grid.minor = element_line(size=0),
      rect = element_rect(size=0),
      legend.text = element_text(size=10),
      legend.position="right",
      legend.title = element_text(size=12, face="bold", vjust = 0.3),
      strip.text = element_text(face="bold")
    ) +

    coord_equal()

  if (is.na(mytitle)) return(myplot) else
    return (myplot + ggtitle(mytitle))
}


# -----------------------------------------------------------------------------
#' @title Plot Method For "mfa" Object
#'
#' @description A plotting function that, given two components/dimensions,
#'   displays a graphic of one of the following: \cr
#'   \itemize{
#'     \item Compromise/Common Factor Scores
#'     \item Partial Factor Scores
#'     \item Loadings
#'     \item Eigenvalues
#'     \item Compromise + Partial Factor Scores
#'     \item Bootstrap ratio plots
#'   }
#'
#' @param x  An object of class "mfa".
#'
#' @param type  Indicates what type of plot the user wishes to see. Must be one
#'   of the following character strings: "compromise", "partial.factor", or
#'   "loadings".
#'
#' @param mytitle An optional parameter for the user to choose the plot title.
#'   By default, mytitle is NA. If NA, the plot is given a title corresponding
#'   to its type, viz "compromise", "partial.factor", or "loadings."
#'
#' @param xdim,ydim The two components the user wishes to plot. Numeric values.
#'
#' @param facetrows  Used with "partial.factor" and "loadings" plots. Controls
#'   how many rows to use when displaying multiple sub-plots.
#'
#' @param size  Controls the size of the plotted points. If plotted points
#'   overlap, the user is encouraged to try reducing size.
#'
#' @param legend An optional parameter that allows the user to control legend
#'   text. Default value is NA. If NA, legend text will be chosen
#'   automatically, based on data row or column names, depending on the plot
#'   type.
#'
#' @param subtabs  Used with "partial.factor" and "loadings" plots. Allows the
#'   user to choose which sub-tables she/he wants to see plots for. Default is
#'   NULL, which will display all the subtables. If not NULL, must be a numeric
#'   vector. each element must be between 1 and K, where K is the total number
#'   of sub-tables in the analysis.
#'
#' @param label  Used with "compromise" and "compromise.partial" plots. Allows the
#'   user to choose which values can be presented as a label on the plot. Default is
#'   NULL, which will display no label. If not NULL, must be a vector.
#'
#' @param bootstrap_size Used only with "bootstrap" plot to control the bootstrap size. Default
#'   value is 1000.
#'
#' @param bootstrap_comps Used only with "bootstrap" plot. Allows the user to chosose which
#' components of bootstrap result she/he want to see plots for. Default is c(1,2), which will
#' display component 1 and 2.
#'
#' @return  Displays the plot of the user's choice.
#'
#' @export
#'
#' @examples
#' # Create an mfa object.
#' sets.num <- list(c(1:6), c(7:12), c(13:18), c(19:23), c(24:29), c(30:34),
#'   c(35:38), c(39:44), c(45:49), c(50:53))
#' mfa1 <- mfa(winedata, sets.num)
#'
#' # Different types of plots:
#' plot(mfa1, type = "compromise", legend=substr(rownames(mfa1$Fcommon),1,2),
#' label=substr(rownames(mfa1$Fcommon),3,3))
#' plot(mfa1, type = "partial.factor",  subtabs = NULL, xdim = 2, ydim = 3, size = 4,
#' legend=substr(rownames(mfa1$Fpartial[[1]]),1,2), label=substr(rownames(mfa1$Fcommon),3,3))
#' plot(mfa1, type = "loadings", size = 2.5, subtabs = c(9,10),
#' legend = c("cat pee", "passion fruit", "green pepper", "mineral","optional 1", "optional 2")))
#' plot(mfa1, type = "eigenvalues")
#' plot(mfa1, type = "compromise.partial", xdim = 1, ydim = 2, l
#' egend=substr(rownames(mfa1$Fcommon),1,2),label=substr(rownames(mfa1$Fcommon),3,3))
#' plot(mfa1, type= "bootstrap", bootstrap_size = 1000, bootstrap_comps=c(1,2), facetrows=2)
#'
plot.mfa <- function(x, type, xdim = 1, ydim = 2, facetrows = 2, size = 5, subtabs = NULL,
                     legend = NA, mytitle = NA, label = NA,
                     bootstrap_size=1000, bootstrap_comps = c(1,2)){
  #

  # Currently the only supported internal plotting function is myplot1
  plotfn <- myplot1

  # Check the validity of the user's choice for subtabs
  if (!is.null(subtabs)){
    stop.isvector(subtabs)
    if (!is.numeric(subtabs)) stop("subtabs must be a numeric vector")
    if (mean(subtabs %in% seq.int(x$K)) != 1)
      stop("each element of subtabs must be an integer between 1 and K,
           where K is the # of sub-tables in the analysis data")
  } else if (is.null(subtabs)) subtabs <- seq.int(x$K)

  # Check that 'type' is one of the five choices
  if (!(type %in% c("compromise", "partial.factor", "loadings",
                    "eigenvalues","compromise.partial","bootstrap") ))
    stop('type must be one of the follonwing:
         compromise, partial.factor, or loadings')

  # Check that the user inputs for dimensions are valid
  if (xdim == ydim) stop("The x- and y-dimensions must be different")
  for (dim in c(xdim, ydim)){
    stop.isvector(dim, 1)
    if (dim != as.integer(dim)) stop("dimensions must be integers")
    if (dim < 1 | dim > x$comps_gen)
      stop("dimensions must be between 1 and the # of components")
  }


  ### 1. compromise/common factors
  if (type == 'compromise'){
    plot_dat <- as.data.frame(x$Fcommon)
    plot_dat$plotID = rownames(x$Fcommon)

    if(sum(is.na(legend))==0) plot_dat$plotID <- legend

    if (!(mean(is.na(legend)) %in% c(0, 1)))
      stop("legend must either be NA or a vector with no NA elements")
    if (mean(is.na(legend)) == 1) legend = sort(plot_dat$plotID)

    if (is.na(mytitle)) mytitle <- "Common/Compromise Factor Scores"

    if(sum(is.na(label))==0) {
      plot_dat$label <- label
      plot.compromise <- plotfn(dat = plot_dat, size = size,
                                xdim = xdim, ydim = ydim, unit_length = 0.3, mytitle = mytitle,
                                legend = legend) +
        geom_text(aes(label=label), vjust=0.5, size=size-1, fontface="bold")
    }
    else {
      plot.compromise <- plotfn(dat = plot_dat, size = size,
                                xdim = xdim, ydim = ydim, unit_length = 0.3, mytitle = mytitle,
                                legend = legend)
    }

    return(plot.compromise)
  }


  ### 2. partial factors
  if (type == 'partial.factor'){

    # Put sub-table factor scores info into one big data.frame
    pf_ag <- data.frame()
    subtabs <- sort(subtabs)
    for (i in subtabs){
      nrow <- nrow(x$Fpartial[[i]])
      pf_ag <- rbind.data.frame(
        pf_ag,
        data.frame(
          x$Fpartial[[i]],
          table = rep(paste("sub-table",i,sep=""),nrow),
          plotID = rownames(x$Fpartial[[i]])))
    }
    if(sum(is.na(legend))==0) pf_ag$plotID <- legend

    if (!(mean(is.na(legend)) %in% c(0, 1)))
      stop("legend must either be NA or a vector with no NA elements")
    if (mean(is.na(legend)) == 1) legend = sort(pf_ag$plotID)

    if (is.na(mytitle)) mytitle <- "Partial Factor Scores"

    # Display all the selected plots using ggplot facets
    if(sum(is.na(label))==0) {
      pf_ag$label <- label
      plot.partial.factor <- plotfn(dat = pf_ag, size = size,
                                    xdim = xdim, ydim = ydim, unit_length = 0.1, mytitle = mytitle,
                                    legend = legend) +
        geom_text(aes(label=label), vjust=0.5, size=size-1, fontface="bold") +
        facet_wrap(~ table, nrow = facetrows)
    }
    else {
      plot.partial.factor <- plotfn(dat = pf_ag, size = size,
                                    xdim = xdim, ydim = ydim, unit_length = 0.1, mytitle = mytitle,
                                    legend = legend) +
        facet_wrap(~ table, nrow = facetrows)
    }
    return(plot.partial.factor)

  }

  ### 3. Loadings
  if (type == "loadings"){

    od <- vector()
    l <- 0
    for (i in 1:x$K){
      od[(l+1):(l+length(x$sets[[i]]))] <-rep(paste0("sub-table", formatC(i, width=2, flag="0")),
                                              length(x$sets[[i]]))
      l <- l + length(x$sets[[i]])
    }

    k <- as.vector(sapply(names(x$origdata),
                          function(x) {
                            if(regexpr("\\.[^\\.]*$",x)>0)
                              substr(x,1,regexpr("\\.[^\\.]*$", x, perl=TRUE)-1)
                            else
                              x
                          }
    ))
    k <- k[1:l]
    subtabs <- sort(subtabs)
    ml_ag <- data.frame(x$Q, variables = k, order=od)
    ml_ag <- ml_ag[as.numeric(substr(ml_ag$order,10,11)) %in% as.numeric(subtabs),]

    col.counts <- sapply(x$sets[subtabs], length)

    ml_ag$plotID <- paste0("var", unlist(unlist(sapply(col.counts,seq.int))))

    if(sum(is.na(legend))==0) ml_ag$plotID <- unlist(sapply(col.counts, function(x) legend[1:x]))

    if (!(mean(is.na(legend)) %in% c(0, 1)))
      stop("legend must either be NA or a vector with no NA elements")

    if (mean(is.na(legend)) == 1) legend = sort(ml_ag$plotID)

    if (is.na(mytitle)) mytitle <- "Loadings"

    # plot the loadings for each sub-table separately
    # NB: var j (j from 1-6) means 'the Jth variable used in that table'
    plot.loadings <- plotfn(dat = ml_ag, size = size,
                            xdim = xdim, ydim = ydim,
                            unit_length = 0.1, mytitle = mytitle,
                            legend = legend) +
      facet_wrap(~ order, nrow = facetrows)
    return(plot.loadings)

  }

  ### 4. Eigenvalues
  if (type == "eigenvalues"){

    message("Eigenvalue plot only uses first 2 arguments.")

    plot.eigen <-
      ggplot(data = data.frame(component=c(1:length(x$eigvals)),eigenvalue=x$eigvals),
             aes(x=component, y=eigenvalue, label=round(x$eigvals,digits=2),
                 xmin=1, xmax=length(x$eigvals), ymax=max(x$eigvals)+0.1)) +
      ggtitle("Eigenvalues") +
      labs(x = "x axis: comps",
           y = "y axis: eigenvalues") +
      geom_bar(stat="identity", colour="grey", fill="white") +
      geom_text(vjust=-0.5, size=3, fontface="bold") +
      theme_bw() +
      theme(axis.title = element_text(size=12, face="bold"),
            axis.text.y = element_text(size=8),
            axis.text.x = element_text(size=8),
            axis.ticks = element_line(size=0),
            plot.title = element_text(size=15, face="bold", hjust=0.5),
            panel.grid.major = element_line(size=0),
            panel.grid.minor = element_line(size=0)) +
      scale_x_discrete(limits =seq(1:length(x$eigvals)))

    return(plot.eigen)
  }

  ### 5. Partial + Compromise
  if (type == "compromise.partial"){
    # 5-1. Partial + Segment
    #Aggregation
    pf_ag2 <- data.frame()
    for (i in 1:10){
      nrow <- nrow(x$Fpartial[[i]])
      pf_ag2 <- rbind.data.frame(
        pf_ag2,
        data.frame(
          x$Fpartial[[i]],
          plotID = rownames(x$Fpartial[[i]])
        )
      )
    }

  #Merge
  pf_f_ag <- merge(pf_ag2[, c(xdim, ydim, length(pf_ag2))],
                   data.frame(x$Fcommon[, c(xdim,ydim)],
                              plotID = rownames(x$Fcommon)), by ="plotID")

  if(sum(is.na(legend))==0) pf_ag2$plotID <- legend

  if (!(mean(is.na(legend)) %in% c(0, 1)))
    stop("legend must either be NA or a vector with no NA elements")

  if (mean(is.na(legend)) == 1) legend = sort(unique(pf_ag2$plotID))

  if (is.na(mytitle)) mytitle <- "Compromise + Partial Factor Scores"

  plot.part <- plotfn(dat = pf_ag2, size = max(size-7,1),
                      xdim = xdim, ydim = ydim, unit_length = 0.3, mytitle = mytitle,
                      legend = legend) +
    geom_segment(data = pf_f_ag,x=pf_f_ag[,2],y=pf_f_ag[,3],xend=pf_f_ag[,4],yend= pf_f_ag[,5],
                 colour="grey", linetype=3)

  #Compromise
  plot_dat <- as.data.frame(x$Fcommon)
  plot_dat$plotID = rownames(x$Fcommon)

  if(sum(is.na(legend))==0) plot_dat$plotID <- legend

  if (!(mean(is.na(legend)) %in% c(0, 1)))
    stop("legend must either be NA or a vector with no NA elements")
  if (mean(is.na(legend)) == 1) legend = sort(plot_dat$plotID)

  if(sum(is.na(label))==0) {
    plot_dat$label <- label
    lth <- length(plot_dat)
    plot.cf <- plot.part +
      geom_point(data = plot_dat,
                 aes(x = get(paste("V",xdim,sep="")), y = get(paste("V",ydim,sep="")),
                     colour=plotID), size = size) +
      geom_text(data = plot_dat,
                aes(x = get(paste("V",xdim,sep="")), y = get(paste("V",ydim,sep="")),
                    label=label), vjust=0.5, size=size-1, fontface="bold")
  }
  else {
    plot.cf <- plot.part +
      geom_point(data = plot_dat,
                 aes(x = get(paste("V",xdim,sep="")), y = get(paste("V",ydim,sep="")),
                     colour=plotID), size = size)
  }

  return(plot.cf)
}

  ### 6. Bootstrap
  if (type == "bootstrap"){

    message("Bootstrap sample only creates with Partial Factor scores.")

    dat_boot <- data.frame(bootstrap(x$Fpartial,bootstrap_size)$t_value)
    bootstrap_comps <- sort(bootstrap_comps)

    dat_boot2 <- data.frame()

    for (i in bootstrap_comps){
      nrow <- length(dat_boot[,i])
      dat_boot2 <- rbind.data.frame(
        dat_boot2,
        data.frame(
          tvalue = dat_boot[,i],
          table = rep(paste("comp",i,sep=""),nrow),
          rownames = rownames(dat_boot)))
    }

    plot.boot <-
      ggplot(data = dat_boot2, aes(x=rownames, y = tvalue, label=rownames,
                             fill=cut(tvalue,breaks = c(-Inf,-3,0,3,Inf)))) +
      geom_bar(stat='identity') +
      coord_flip() +
      geom_text(size=3, fontface="bold") +
      scale_x_discrete(limits = unique(rev(dat_boot2$rownames))) +
      scale_fill_manual(breaks = c("-3", "0", "3"), values = c("blue","grey","grey","purple")) +
      facet_wrap(~table, nrow = facetrows) +
      ggtitle("Bootstrap ratio plots") +
      labs(x = "y axis: obs",
           y = "x axis: t_value") +
      theme_bw() +
      theme(axis.title = element_text(size=12, face="bold", hjust=0.5),
            axis.text.y = element_text(size=0),
            axis.text.x = element_text(size=8),
            axis.ticks = element_line(size=0),
            plot.title = element_text(size=15, face="bold", hjust=0.5),
            panel.grid.major = element_line(size=0),
            panel.grid.minor = element_line(size=0),
            strip.text = element_text(size=12, face="bold"))

    return(plot.boot)
  }

}
