\name{tidycamera}
\alias{tidycamera}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Data formatting
}
\description{
The function returns a  dataframe by species by independent photos and make data easily to go.
}
\usage{
tidycamera(data, gridID, cameraID, year, species, date_time, End_Date, Start_Date, interval = 30)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
should be a dataframe or a tibble.
}
  \item{gridID}{
which column is gridID.
}
  \item{cameraID}{
which column is cameraID.
}
  \item{year}{
which column is year.
}
  \item{species}{
which column is species.
}
  \item{date_time}{
which colunmn is date_time (the date and time of a given photo)
}
  \item{End_Date}{
which colunmn is the End_Date (the ends of a survey).
}
  \item{Start_Date}{
which colunmn is the Start_Date (the starts of a survey).
}
  \item{interval}{
the threshold of interval between two independent photos.Typically, is 30 mins (default).
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
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(testdata)
res <- tidycamera(testdata,gridID="调查点名称",cameraID="调查点名称",
                      year="Year",species="中文名",date_time="矫正开始时间",
                      End_Date="矫正结束日期",Start_Date="矫正起始日期",interval=30)
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
