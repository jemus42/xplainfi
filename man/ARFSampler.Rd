% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FeatureSampler.R
\name{ARFSampler}
\alias{ARFSampler}
\title{ARF-based Conditional Sampler}
\description{
Implements conditional sampling using Adversarial Random Forests (ARF).
ARF can handle mixed data types (continuous and categorical) and provides
flexible conditional sampling by modeling the joint distribution.
}
\details{
The ARFSampler fits an \link[arf:arf-package]{Adversarial Random Forest} model on the task data,
then uses it to generate samples from \eqn{P(X_j | X_{-j})} where \eqn{X_j} is the
feature of interest and \eqn{X_{-j}} are the conditioning features.
}
\examples{
\dontshow{if (requireNamespace("arf", quietly = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
library(mlr3)
task = tgen("2dnormals")$generate(n = 100)
# Create sampler with default parameters
sampler = ARFSampler$new(task, conditioning_set = "x2", verbose = FALSE)
data = task$data()
# Will use the stored parameters
sampled_data = sampler$sample("x1", data)

# Example with custom parameters
sampler_custom = ARFSampler$new(task, round = FALSE)
sampled_custom = sampler_custom$sample("x1", data)
\dontshow{\}) # examplesIf}
}
\references{
Watson, S. D, Blesch, Kristin, Kapar, Jan, Wright, N. M (2023).
\dQuote{Adversarial Random Forests for Density Estimation and Generative Modeling.}
In \emph{Proceedings of The 26th International Conference on Artificial Intelligence and Statistics}, 5357--5375.
\url{https://proceedings.mlr.press/v206/watson23a.html}.

Blesch, Kristin, Koenen, Niklas, Kapar, Jan, Golchian, Pegah, Burk, Lukas, Loecher, Markus, Wright, N. M (2025).
\dQuote{Conditional Feature Importance with Generative Modeling Using Adversarial Random Forests.}
\emph{Proceedings of the AAAI Conference on Artificial Intelligence}, \bold{39}(15), 15596--15604.
\doi{10.1609/aaai.v39i15.33712}.
}
\section{Super classes}{
\code{\link[xplainfi:FeatureSampler]{xplainfi::FeatureSampler}} -> \code{\link[xplainfi:ConditionalSampler]{xplainfi::ConditionalSampler}} -> \code{ARFSampler}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{arf_model}}{Adversarial Random Forest model}

\item{\code{psi}}{Distribution parameters estimated from ARF}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ARFSampler-new}{\code{ARFSampler$new()}}
\item \href{#method-ARFSampler-sample}{\code{ARFSampler$sample()}}
\item \href{#method-ARFSampler-clone}{\code{ARFSampler$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ARFSampler-new"></a>}}
\if{latex}{\out{\hypertarget{method-ARFSampler-new}{}}}
\subsection{Method \code{new()}}{
Creates a new instance of the ARFSampler class.
To fit the ARF in parallel, set \code{arf_args = list(parallel = TRUE)} and register a parallel backend (see \link[arf:arf-package]{arf::arf}).
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ARFSampler$new(
  task,
  conditioning_set = NULL,
  finite_bounds = "no",
  round = TRUE,
  stepsize = 0,
  verbose = FALSE,
  parallel = FALSE,
  arf_args = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{task}}{(\link[mlr3:Task]{mlr3::Task}) Task to sample from}

\item{\code{conditioning_set}}{(\code{character} | \code{NULL}) Default conditioning set to use in \verb{$sample()}. This parameter only affects the sampling behavior, not the ARF model fitting.}

\item{\code{finite_bounds}}{(\code{character(1)}: \code{"no"}) Passed to \code{arf::forde()}. Default is \code{"no"} for compatibility. \code{"local"} may improve extrapolation but can cause issues with some data.}

\item{\code{round}}{(\code{logical(1)}: \code{TRUE}) Whether to round continuous variables back to their original precision.}

\item{\code{stepsize}}{(\code{numeric(1)}: \code{0}) Number of rows of evidence to process at a time wehn \code{parallel} is \code{TRUE}. Default (\code{0}) spreads evidence evenly over registered workers.}

\item{\code{verbose}}{(\code{logical(1)}: \code{FALSE}) Whether to print progress messages. Default is \code{FALSE} but default in \code{arf} is \code{TRUE}.}

\item{\code{parallel}}{(\code{logical(1)}: \code{FALSE}) Whether to use parallel processing via \code{foreach}. See examples in \code{\link[arf:forge]{arf::forge()}}.}

\item{\code{arf_args}}{(\link{list}) Additional passed to \code{arf::adversarial_rf}.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ARFSampler-sample"></a>}}
\if{latex}{\out{\hypertarget{method-ARFSampler-sample}{}}}
\subsection{Method \code{sample()}}{
Sample values for feature(s) conditionally on other features using ARF
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ARFSampler$sample(
  feature,
  data = self$task$data(),
  conditioning_set = NULL,
  round = NULL,
  stepsize = NULL,
  verbose = NULL,
  parallel = NULL,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{feature}}{(\code{character}) Feature(s) of interest to sample (can be single or multiple)}

\item{\code{data}}{(\code{\link[data.table:data.table]{data.table}}) Data containing conditioning features. Defaults to \verb{$task$data()}, but typically a dedicated test set is provided.}

\item{\code{conditioning_set}}{(\code{character(n) | NULL}) Features to condition on. If \code{NULL}, uses the stored parameter if available, otherwise defaults to all other features.}

\item{\code{round}}{(\code{logical(1) | NULL}) Whether to round continuous variables. If \code{NULL}, uses the stored parameter value.}

\item{\code{stepsize}}{(\code{numeric(1) | NULL}) Step size for variance adjustment. If \code{NULL}, uses the stored parameter value.}

\item{\code{verbose}}{(\code{logical(1) | NULL}) Whether to print progress messages. If \code{NULL}, uses the stored parameter value.}

\item{\code{parallel}}{(\code{logical(1) | NULL}) Whether to use parallel processing. If \code{NULL}, uses the stored parameter value.}

\item{\code{...}}{Further arguments passed to \code{arf::forge()}.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Modified copy of the input data with the feature(s) sampled conditionally
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ARFSampler-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ARFSampler-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ARFSampler$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
