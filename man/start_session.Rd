\name{start_session}
\alias{start_session}
\title{Create a new Webdriver session}
\usage{
  start_session(root = NULL, browser = "firefox",
    javascriptEnabled = TRUE, takesScreenshot = TRUE,
    handlesAlerts = TRUE, databaseEnabled = TRUE,
    cssSelectorsEnabled = TRUE)
}
\arguments{
  \item{root}{a string giving the Java-Server IP}

  \item{browser}{a string giving the browser under which
  the session is started}

  \item{takesScreenshot}{a logical value indicating whether
  the session supports taking screenshots of the current
  page}

  \item{handlesAlerts}{a logical value indicating whether
  the session can interact with modal popups, such as
  window.alert and window.confirm}

  \item{databaseEnabled}{a logical value indicating whether
  the session can interact database storage}

  \item{cssSelectorsEnabled}{a logical value indicating
  whether the session supports CSS selectors when searching
  for elements}

  \item{javascriptEnabled}{a logical value indicating
  whether the session supports executing user supplied
  JavaScript in the context of the current page}
}
\value{
  A list describing the session's capabilities.
}
\description{
  This function creates a new Webdriver session with
  specificed capabilities
}

