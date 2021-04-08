\name{split.year}
\alias{split.year}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Split date by year
}
\description{
In case of that if your survey period was crossing years, but you want to Split it by year.
}
\usage{
split.year(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
a formatted dataframe or tibble by function tidycamera
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a  dataframe by species by independent photos.
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

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 objects to See Also as \code{\link{tidycamera}}
}
\examples{
data(testdata)
res <- tidycamera(testdata,gridID="调查点名称",cameraID="调查点名称",
                      year="Year",species="中文名",date_time="矫正开始时间",
                      End_Date="矫正结束日期",Start_Date="矫正起始日期",interval=30)

res <- split.year(res)
str(res)
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
