% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PerturbationImportance.R
\name{PFI}
\alias{PFI}
\title{Permutation Feature Importance}
\description{
Implementation of Permutation Feature Importance (PFI) using modular sampling approach.
PFI measures the importance of a feature by calculating the increase in model error
when the feature's values are randomly permuted, breaking the relationship between
the feature and the target variable.
}
\details{
Permutation Feature Importance was originally introduced by Breiman (2001) as part of
the Random Forest algorithm. The method works by:
\enumerate{
\item Computing baseline model performance on the original dataset
\item For each feature, randomly permuting its values while keeping other features unchanged
\item Computing model performance on the permuted dataset
\item Calculating importance as the difference (or ratio) between permuted and original performance
}
}
\examples{
\dontshow{if (requireNamespace("ranger", quietly = TRUE) && requireNamespace("mlr3learners", quietly = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
library(mlr3learners)
task = tgen("xor", d = 5)$generate(n = 100)
pfi = PFI$new(
  task = task,
  learner = lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
  measure = msr("classif.ce"),
  resampling = rsmp("cv", folds = 3),
  iters_perm = 3
)
pfi$compute()
\dontshow{\}) # examplesIf}
}
\references{
Breiman, Leo (2001).
\dQuote{Random Forests.}
\emph{Machine Learning}, \bold{45}(1), 5--32.
\doi{10.1023/A:1010933404324}.
Fisher, Aaron, Rudin, Cynthia, Dominici, Francesca (2019).
\dQuote{All Models Are Wrong, but Many Are Useful: Learning a Variable's Importance by Studying an Entire Class of Prediction Models Simultaneously.}
\emph{Journal of Machine Learning Research}, \bold{20}, 177.
\url{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8323609/}.
Strobl, Carolin, Boulesteix, Anne-Laure, Kneib, Thomas, Augustin, Thomas, Zeileis, Achim (2008).
\dQuote{Conditional Variable Importance for Random Forests.}
\emph{BMC Bioinformatics}, \bold{9}(1), 307.
ISSN 1471-2105, \doi{10.1186/1471-2105-9-307}.
}
\section{Super classes}{
\code{\link[xplainfi:FeatureImportanceMethod]{xplainfi::FeatureImportanceMethod}} -> \code{\link[xplainfi:PerturbationImportance]{xplainfi::PerturbationImportance}} -> \code{PFI}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-PFI-new}{\code{PFI$new()}}
\item \href{#method-PFI-compute}{\code{PFI$compute()}}
\item \href{#method-PFI-clone}{\code{PFI$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="xplainfi" data-topic="FeatureImportanceMethod" data-id="combine"><a href='../../xplainfi/html/FeatureImportanceMethod.html#method-FeatureImportanceMethod-combine'><code>xplainfi::FeatureImportanceMethod$combine()</code></a></span></li>
<li><span class="pkg-link" data-pkg="xplainfi" data-topic="FeatureImportanceMethod" data-id="print"><a href='../../xplainfi/html/FeatureImportanceMethod.html#method-FeatureImportanceMethod-print'><code>xplainfi::FeatureImportanceMethod$print()</code></a></span></li>
<li><span class="pkg-link" data-pkg="xplainfi" data-topic="FeatureImportanceMethod" data-id="reset"><a href='../../xplainfi/html/FeatureImportanceMethod.html#method-FeatureImportanceMethod-reset'><code>xplainfi::FeatureImportanceMethod$reset()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PFI-new"></a>}}
\if{latex}{\out{\hypertarget{method-PFI-new}{}}}
\subsection{Method \code{new()}}{
Creates a new instance of the PFI class
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PFI$new(
  task,
  learner,
  measure,
  resampling = NULL,
  features = NULL,
  relation = "difference",
  iters_perm = 1L
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{task, learner, measure, resampling, features}}{Passed to PerturbationImportance}

\item{\code{relation}}{(character(1)) How to relate perturbed scores to originals. Can be overridden in \verb{$compute()}.}

\item{\code{iters_perm}}{(integer(1)) Number of permutation iterations. Can be overridden in \verb{$compute()}.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PFI-compute"></a>}}
\if{latex}{\out{\hypertarget{method-PFI-compute}{}}}
\subsection{Method \code{compute()}}{
Compute PFI scores
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PFI$compute(relation = NULL, iters_perm = NULL, store_backends = TRUE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{relation}}{(character(1)) How to relate perturbed scores to originals. If \code{NULL}, uses stored value.}

\item{\code{iters_perm}}{(integer(1)) Number of permutation iterations. If \code{NULL}, uses stored value.}

\item{\code{store_backends}}{(logical(1)) Whether to store backends}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PFI-clone"></a>}}
\if{latex}{\out{\hypertarget{method-PFI-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PFI$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
