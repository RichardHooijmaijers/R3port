#------------------------------------------ html_plot ------------------------------------------
#' Prints a R plot to a HTML file or console
#'
#' This function makes a HTML document including the defined plots
#'
#' @param plot plot object or function call that creates plot to be printed to file
#' @param out filename for the output HTML file
#' @param title character string to define the title of the table which will be added to the caption
#' @param titlepr character string to define the prefix of the table title. Can be used to create custom table numbering
#' @param footnote character string with the footnote to be placed in the footer of the page (HTML coding can be used for example to create line breaks)
#' @param pwidth numeric indicating the width of the plot to be generated in pixels
#' @param pheight numeric indicating the height of the plot to be generated in pixels
#' @param res numeric indicating the resolution of the plot, if set to NULL it will adapt the value according height of the plot
#' @param fontsize character string with the default font or pointsize passed through to png function
#' @param units character string with the units to use for plot width and height passed through to png function
#' @param rawout character string with the name of the raw HTML file to generate (e.g. only plotting code)
#'    In case NULL no raw output will be generated. In order to combine results the filename should end in .raw.html
#' @param cleancur logical indicating if the available plots should be deleted before creating new ones
#' @param ... additional arguments passed through to \code{\link{html_doc}}. Most important are template, rendlist, css and show
#'
#' @return The function returns a HTML file (or writes output to console)
#'
#' @export
#' @examples
#'
#' # It is convenient to have an object for the plot argument
#' \dontrun{
#'
#'   data(Theoph)
#'   library(ggplo2)
#'   pl <- qplot(Time, conc, data=Theoph, facets=~Subject,geom="line")
#'   html_plot(pl,out=paste0(tempfile(),".html"))
#'
#'   # Base plots work a bit different and can be placed
#'   # in the function directly or wrapped in a function
#'   html_plot(plot(conc~Time,data=Theoph),out=tempfile(fileext=".html"))
#'   pl <- function() {
#'     plot(conc~Time,data=Theoph)
#'     title(main="a plot")
#'   }
#'   html_plot(pl(),out=tempfile(fileext=".html"))
#' }
html_plot <- function(plot,out,title="plot",titlepr=NULL,footnote="",pwidth=1000,pheight=600,res=NULL,
                      fontsize=12,units="px",rawout=paste0(out,".rawhtml"),cleancur=FALSE,...){
  if(is.null(out)|out=="") stop("A valid name for the output should be specified")

  # Set logics for option system
  if(!is.null(getOption('pwidth')))   pwidth   <- getOption('pwidth')
  if(!is.null(getOption('pheight')))  pheight  <- getOption('pheight')

  # Delete figure files when specified
  if(cleancur){
    dirnm <- list.files(paste0(dirname(out),"/figures"),full.names = TRUE)
    dirnm <- dirnm[grepl(paste0("^",basename(tools::file_path_sans_ext(out)),"[[:digit:]]{3}\\.png"),basename(dirnm))]
    if(length(dirnm)>0) try(file.remove(dirnm),silent=TRUE)
  }

  # Create subfolder to place graphs in
  dir.create(paste(dirname(out),"figures",sep="/"),showWarnings = FALSE)

  # Save plot to location. For now only png is selected as this is the best format for HTML
  prpl <- function(){
    reso   <- ifelse(is.null(res) & units=="px",as.numeric(pheight)/6,ifelse(is.null(res) & units!="px",200,res))
    grDevices::png(filename=paste0(dirname(out),"/figures/",sub("\\.html$","",basename(out)),"%03d.png"),width=pwidth,height=pheight,res=reso,pointsize=fontsize,units=units)
    print(plot)
    grDevices::dev.off()
  }
  tplot <- try(prpl())
  if("try-error"%in%class(tplot)){
    if(!is.null(grDevices::dev.list())) grDevices::dev.off()
    stop("Cannot create the plot")
  }

  # Create the HTML plot code
  # Multiple plots are implemented where the title is not repeated.
  numplots <-  list.files(paste0(dirname(out),"/figures/"),pattern=paste0("^",tools::file_path_sans_ext(basename(out)),"[[:digit:]]{3}\\.png"))
  plt  <- NULL
  for(i in 1:length(numplots)){
    if(i==1) plt <- c(plt,paste("<h1>",titlepr,title,"</h1>"))
    plt <- c(plt,paste0("<img src='./figures/",paste0(sub("\\.html$","",basename(out)),formatC(i,width=3,flag="0"),".png" ),"' alt='Oops something went wrong, check your code' class='img-resp'>"))
    if(!is.null(footnote) & i==length(numplots)) plt <- c(plt,"<br/>",footnote)
  }

  # Print the plot
  html_doc(plt,out=out,...)
  if(!is.null(rawout) & !dir.exists(dirname(rawout))){
    succ <- try(dir.create(dirname(rawout),showWarnings = FALSE))
    if(!succ) stop("Output folder for raw files cannot be created")
  }
  if(!is.null(rawout)) cat(plt,sep="\n",file=rawout)
}
