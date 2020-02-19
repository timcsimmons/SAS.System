\name{SAS.System}
\alias{SAS.System}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract data from SAS libraries
}
\description{
This function returns an environment that defines methods for defining and
interacting with SAS libraries.
}
\usage{
SAS.System()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns an environment that contains a data frame of known formats and a function
called libname. Libname takes a valid libref and its path and added a reference
to it in the environment. After using libname, the libref is a list that contains
several memebers:

  \item{tables }{Returns the names of the tables in the libref}
  \item{contents }{Given the name of data set as a string, returns a data frame of the contents of a data set}
  \item{data }{Given the name of data set as a string, fetches a data set from libref and applies known formats to obtain factors}
  \item{path }{The path associated with the libref}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Tim Simmons
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

## Define locations of the project and manuscript
assess <- "P:\\\\PECARN\\\\ASSESS\\\\Statistical Analysis\\\\"
manuscript <- "Manuscripts\\\\Mental health symptoms\\\\"
msd.location <- paste0(assess, manuscript, "Data\\\\Derived\\\\Lead")


SAS <- SAS.System()

SAS$libname("raw", paste0(assess, "Data\\\\Raw"))
SAS$libname("der", paste0(assess, "Data\\\\Derived\\\\Lead"))
SAS$libname("msd", msd.location)
SAS$libname("mso", paste0(assess, "Programs\\\\Lead\\\\", manuscript, "Output"))

SAS$msd$tables()
SAS$msd$contents("risk")[1:3]

risk <- SAS$msd$data("risk")
head(risk)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
