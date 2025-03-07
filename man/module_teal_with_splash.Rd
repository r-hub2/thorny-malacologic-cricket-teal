% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/module_teal_with_splash.R
\name{module_teal_with_splash}
\alias{module_teal_with_splash}
\alias{ui_teal_with_splash}
\alias{srv_teal_with_splash}
\title{UI and server modules of \code{teal}}
\usage{
ui_teal_with_splash(
  id,
  data,
  modules,
  title = build_app_title(),
  header = tags$p(),
  footer = tags$p()
)

srv_teal_with_splash(id, data, modules, filter = teal_slices())
}
\arguments{
\item{id}{(\code{character(1)}) \code{shiny} module instance id.}

\item{data}{(\code{teal_data}, \code{teal_data_module}, or \code{reactive} returning \code{teal_data})
The data which application will depend on.}

\item{modules}{(\code{teal_modules})
\code{teal_modules} object. These are the specific output modules which
will be displayed in the \code{teal} application. See \code{\link[=modules]{modules()}} and \code{\link[=module]{module()}} for
more details.}

\item{title}{(\code{shiny.tag} or \code{character(1)}) \ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}} Optionally,
the browser window title. Defaults to a title "teal app" with the icon of NEST.
Can be created using the \code{build_app_title()} or
by passing a valid \code{shiny.tag} which is a head tag with title and link tag.
This parameter is no longer supported. Use \code{modify_title()} on the teal app object instead.}

\item{header}{(\code{shiny.tag} or \code{character(1)}) \ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}} Optionally,
the header of the app.
This parameter is no longer supported. Use \code{modify_header()} on the teal app object instead.}

\item{footer}{(\code{shiny.tag} or \code{character(1)}) \ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}} Optionally,
the footer of the app.
This parameter is no longer supported. Use \code{modify_footer()} on the teal app object instead.}

\item{filter}{(\code{teal_slices}) Optionally,
specifies the initial filter using \code{\link[=teal_slices]{teal_slices()}}.}
}
\value{
Returns a \code{reactive} expression containing a \code{teal_data} object when data is loaded or \code{NULL} when it is not.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}
Please use \code{\link{module_teal}} instead.
}
