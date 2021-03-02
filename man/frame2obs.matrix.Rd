\name{frame2obs.matrix}
\alias{frame2obs.matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
occupany model data
}
\description{
returns an objects for occupancy modeling.
}
\usage{
frame2obs.matrix(data, varnames = c("gridID", "date", "species"), type.out = "array")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
a dataframe formated by obs_df.
}
  \item{varnames}{
the dimentions of data structure.
}
  \item{type.out}{
returns array or dataframe, by default array.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Hai-Dong Li
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
xx <- obs_df(res)
yf <- frame2obs.matrix(xx)
yf

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
