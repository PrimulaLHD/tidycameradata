\name{accu_vegan}
\alias{accu_vegan}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Species Accumulation Curves
}
\description{
To accumulate with camera-trap-days increasing by using vegan package.
}
\usage{
accu_vegan(data, method = "random", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
a data frame.
}
  \item{method}{
see vegan package.
}
  \item{\dots}{
see vegan package.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
returns a dataframe.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
 https://cran.r-project.org, https://github.com/vegandevs/vegan
}
\author{
Hai-Dong Li
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
objects to See Also as \code{\link{accu_hill}}
}
\examples{
accumu_vegan <- accu_vegan(res)

ggplot(accumu_vegan, aes(x=Camera.trap.days, y=species)) +
    geom_ribbon(aes(ymin=species-sd, ymax=species+sd),fill="grey80", alpha=0.5)+
    geom_line() +
    theme_bw()
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
