\name{accu_hill}
\alias{accu_hill}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Species Accumulation Curves
}
\description{
based on hill number to perform species accumulation curves( see Hsieh, Ma and Chao 2014)
}
\usage{
accu_hill(data, by = "site", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
dataframe
}
  \item{by}{
see iNEXT package
}
  \item{\dots}{
see iNEXT package
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
return an iNEXT object
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Hsieh, T.C., Ma, K.H. & Chao, A. (2016) iNEXT: An R package for interpolation and extrapolation of species diversity (Hill numbers). Methods in Ecology and Evolution, 7, 1451-1456.
}
\author{
Hai-Dong Li
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
objects to See Also as \code{\link{accu_vegan}}
}
\examples{
library(iNEXT)
hill_accu1 <- accu_hill(res,q=c(0),by="total")
hill_accu1
ggiNEXT(hill_accu1, type=1)+theme_bw()+
  xlab("Number of independent photos")
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
