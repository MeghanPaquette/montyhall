% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-problem.R
\name{determine_winner}
\alias{determine_winner}
\title{\preformatted{Did you win the car, or lose and get a goat? 
}}
\usage{
determine_winner(final.pick, game)
}
\arguments{
\item{...}{no arguments are used by the function.}
}
\value{
\preformatted{The function returns a character vector of WIN or LOSE
depending on if the goat or car door is the final door 
pick. 
}
}
\description{
\preformatted{`determine_winner()` returns a character vector of
WIN or LOSE depending on if the final door was the car or goat. 
}
}
\details{
\preformatted{Based on the final door that was picked, the contestant
is deemed to WIN or LOSE based on if the final door that
was selected was the car or one of the goats. 
}
}
\examples{
    determine_winner()
    
}
