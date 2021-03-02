\name{summary_ind_photo}
\alias{summary_ind_photo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
independent photos
}
\description{
the function returns a tibble of the number of independent photos by species by site and so on
}
\usage{
summary_ind_photo(dataframe, groupvars)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataframe}{
a data frame
}
  \item{groupvars}{
a vector contains several variables.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
returns a tibble with the number of independent photos by species.
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
objects to See Also as \code{\link{summary_cam_days}}
}
\examples{
ind_photo <- summary_ind_photo(res,groupvars = c("gridID","year","species") )
ind_photo

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
