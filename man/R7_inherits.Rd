% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inherits.R
\name{R7_inherits}
\alias{R7_inherits}
\alias{check_is_R7}
\title{Does this object inherit from an R7 class?}
\usage{
R7_inherits(x, class)

check_is_R7(x, class = NULL, arg = deparse(substitute(x)))
}
\arguments{
\item{x}{An object}

\item{class}{An R7 class. Can be omitted in \code{check_is_R7()}.}

\item{arg}{Argument name used in error message.}
}
\description{
\itemize{
\item \code{R7_inherits()} returns \code{TRUE} or \code{FALSE}.
\item \code{check_is_R7()} throws an error.
}
}
\examples{
foo1 <- new_class("foo1")
foo2 <- new_class("foo2")

R7_inherits(foo1(), foo1)
check_is_R7(foo1())
check_is_R7(foo1(), foo1)

R7_inherits(foo1(), foo2)
try(check_is_R7(foo1(), foo2))
}
