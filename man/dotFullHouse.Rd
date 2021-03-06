% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/poker.R
\name{dotFullHouse}
\alias{dotFullHouse}
\title{dotFullHouse}
\usage{
dotFullHouse(cards, score)
}
\arguments{
\item{cards}{: \tabular{ll}{the 7 card hand as
matrix[nPlayers, 14] \tab \cr \tab
col1: rank of card 1 in \{2, ... , 14\} \cr \tab
col2: suit of card 1 in \{1, 2, 3, 4\} \cr \tab
col3: rank of card 2 \cr \tab
col4: suit of card 2 \cr \tab
. \cr \tab
. \cr \tab
. \cr \tab
col13: rank of card 7 \cr \tab
col14: suit of card 7}}

\item{score}{: \tabular{ll}{the score of the hand in absolute terms as
vector[nPlayers] \tab \cr \tab
 9 = Straight Flush \cr \tab
 8 = Four of a Kind \cr \tab
 7 = Full House \cr \tab
 6 = Flush \cr \tab
 5 = Straight \cr \tab
 4 = Three of a Kind \cr \tab
 3 = Two Pair \cr \tab
 2 = One Pair \cr \tab
 1 = High Card}}
}
\value{
winner : absolute position of the winner as
											vector
}
\description{
Determine the player with the highest boat.
}
\examples{
cards <- c(5,10,4,8,1,2,1,1,10,9,6,7,3,2,2,2,5,5,5,5,3,3,3,3,8,8,8,8,3,3,3,3)
cards <- c(cards,14,14,14,14,2,2,2,2,14,14,14,14,3,3,3,3,14,14,14,14,4,4,4,4)
cards <- matrix(cards,nrow=4,ncol=14); cards
score <- showdown(cards); score
dotFullHouse(cards,score)

cards <- c(5,10,4,8,1,2,1,1,10,9,6,7,3,2,2,2,12,12,12,12,1,1,1,1,12,12,12,12,3,3,3,3)
cards <- c(cards,14,14,14,14,2,2,2,2,14,14,14,14,3,3,3,3,14,14,14,14,4,4,4,4)
cards <- matrix(cards,nrow=4,ncol=14); cards
score <- showdown(cards); score
dotFullHouse(cards,score)
}
\seealso{
\code{\link{dotFullHouseRanker}}
}
