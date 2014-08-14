\name{free2}
\alias{free2}
\title{Freedom of Speech Data}
\usage{data(free2)}
\description{

  Selection of individual-level survey data for freedom of speech.

  }

\details{ 

  A table with 150 observations and 12 variables. \itemize{
    \item{sex}{1 for men and 0 for women}
    \item{age}{Age of respondent in years}
    \item{educ}{Levels of education, coded as a numeric variable with
      \itemize{
	\item{1}{No formal education}
	\item{2}{Less than primary school education}
	\item{3}{Completed primary school}
	\item{4}{Completed secondary school}
	\item{5}{Completed high school}
	\item{6}{Completed college}
	\item{7}{Completed post-graduate degree}
      }
    }
    \item{country}{Character strings consisting of "Oceana",
      "Eurasia", and "Eastasia", after Orwell's \emph{1984}.}
    \item{y}{Self assessment (see below).}
    \item{v1-v6}{Response to vignettes (see below).}
  }
  Survey respondents were asked in almost the same language for a
  self-assessment and for an assessment of several hypothetical persons
  described by written vignettes.  The self assessment (\code{self}, in
  the data set), "How free do you think [name/you] [is/are] to express
  [him-her/your]self without fear of government reprisal?" was first
  asked of the survey respondent with respect to him or herself, and
  then after each of vignette.  The possible response categories are:  \itemize{
    \item{1}{Completely free}
    \item{2}{Very free}
    \item{3}{Moderately free}
    \item{4}{Slightly free}
    \item{5}{Not free at all}
  }
  The vignettes, ordered from most free to least free, are:
  \itemize{
    \item{vign1}{[Kay] does not like many of the government's
    policies. She frequently publishes her opinion in newspapers,
    criticizing decisions by officials and calling for change. She sees
    little reason these actions could lead to government reprisal.}

    \item{vign2}{[Michael] disagrees with many of the government's
    policies. Though he knows criticism is frowned upon, he doesn't
    believe the government would punish someone for expressing critical
    views. He makes his opinion known on most issues without regard to
    who is listening.}

    \item{vign3}{[Bob] has political views at odds with the
    government. He has heard of people occasionally being arrested for
    speaking out against the government, and government leaders
    sometimes make political speeches condemning those who criticize. He
    sometimes writes letters to newspapers about politics, but he is
    careful not to use his real name.}

    \item{vign4}{[Connie] does not like the government's stance on many
    issues. She has a friend who was arrested for being too openly
    critical of governmental leaders, and so she avoids voicing her
    opinions in public places.}

    \item{vign5}{[Vito] disagrees with many of the government's
    policies, and is very careful about whom he says this to, reserving
    his real opinions for family and close friends only. He knows
    several men who have been taken away by government officials for
    saying negative things in public.}

    \item{vign6}{[Sonny] lives in fear of being harassed for his
    political views. Everyone he knows who has spoken out against the
    government has been arrested or taken away. He never says a word
    about anything the government does, not even when he is at home
    alone with his family. }
  }  
}

\references{
  \emph{WHO's World Health Survey}
    by Lydia Bendib, Somnath Chatterji, Alena Petrakova, Ritu Sadana,
    Joshua A. Salomon, Margie Schneider, Bedirhan Ustun, Maria
    Villanueva

  Jonathan Wand, Gary King and Olivia Lau. (2007) ``Anchors: Software for
  Anchoring Vignettes''. \emph{Journal of Statistical Software}.  Forthcoming.
  copy at http://wand.stanford.edu/research/anchors-jss.pdf

  Gary King and Jonathan Wand.  "Comparing Incomparable Survey
  Responses: New Tools for Anchoring Vignettes," Political Analysis, 15,
  1 (Winter, 2007): Pp. 46-66,
  copy at http://gking.harvard.edu/files/abs/c-abs.shtml.
}
\keyword{datasets}
