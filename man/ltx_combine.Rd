% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ltx_combine.r
\name{ltx_combine}
\alias{ltx_combine}
\title{Combines multiple latex files to a single tex and compiles document}
\usage{
ltx_combine(combine = ".", out = NULL, presentation = FALSE, clean = 0, ...)
}
\arguments{
\item{combine}{character string with the location of the raw tex files or list with file names within same directory}

\item{out}{filename for the output latex file (if an empty string is provided it will print to console)}

\item{presentation}{logical indicating if the output is a latex presentation (in this case the results will be placed
within frames and without captions and clearpage)}

\item{clean}{integer between 0 and 2 indicating if all individual files should be kept (0), all individual tex and raw tex files should
be deleted (1) or all individual files should be deleted (2)}

\item{...}{additional arguments passed through to \code{\link[=ltx_doc]{ltx_doc()}}. Most important are template, rendlist, compile and show}
}
\value{
The function returns a latex file (or writes output to console)
}
\description{
This function combines multiple latex files. This is done based on the name of the files and should
end with raw.tex to make the function pick-up these files.
}
\details{
Currently the generated output is saved in the same place where the separate tables and plots are located
defined in the 'combine' argument. This is done even when a different file path is specified in 'out' (using the basename function).
The reason is to not copy files linked within the document and preventing broken links. This behaviour might change in future releases
}
\examples{

# Take into account the usage of tempfile() with multiple function calls
\dontrun{
  data(Theoph)
  ltx_list(Theoph[1:11,],out=tempfile(fileext=".tex"),show=FALSE)
  ltx_plot(plot(conc~Time,data=Theoph),out=tempfile(fileext=".tex"),show=FALSE)
  ltx_combine(combine=tempdir(),out="rep1.tex")

  # possibility for presentation layout (beamer template provided in package)
  ltx_combine(combine=tempdir(),out="rep1.tex",
              template=paste0(system.file(package="R3port"),"/beamer.tex"),
              presentation=TRUE)

  # Or other template with different orientation
  ltx_combine(combine=tempdir(),out="rep1.tex",
              template=paste0(system.file(package="R3port"),"/listing.tex"),
              orientation="portrait")
}
}
