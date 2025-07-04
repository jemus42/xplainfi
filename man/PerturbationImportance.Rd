% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PerturbationImportance.R
\name{PerturbationImportance}
\alias{PerturbationImportance}
\title{Perturbation Feature Importance Base Class}
\description{
Abstract base class for perturbation-based importance methods PFI, CFI, and RFI
}
\section{Super class}{
\code{\link[xplainfi:FeatureImportanceMethod]{xplainfi::FeatureImportanceMethod}} -> \code{PerturbationImportance}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{sampler}}{(\link{FeatureSampler}) Sampler object for feature perturbation}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-PerturbationImportance-new}{\code{PerturbationImportance$new()}}
\item \href{#method-PerturbationImportance-clone}{\code{PerturbationImportance$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="xplainfi" data-topic="FeatureImportanceMethod" data-id="combine"><a href='../../xplainfi/html/FeatureImportanceMethod.html#method-FeatureImportanceMethod-combine'><code>xplainfi::FeatureImportanceMethod$combine()</code></a></span></li>
<li><span class="pkg-link" data-pkg="xplainfi" data-topic="FeatureImportanceMethod" data-id="compute"><a href='../../xplainfi/html/FeatureImportanceMethod.html#method-FeatureImportanceMethod-compute'><code>xplainfi::FeatureImportanceMethod$compute()</code></a></span></li>
<li><span class="pkg-link" data-pkg="xplainfi" data-topic="FeatureImportanceMethod" data-id="print"><a href='../../xplainfi/html/FeatureImportanceMethod.html#method-FeatureImportanceMethod-print'><code>xplainfi::FeatureImportanceMethod$print()</code></a></span></li>
<li><span class="pkg-link" data-pkg="xplainfi" data-topic="FeatureImportanceMethod" data-id="reset"><a href='../../xplainfi/html/FeatureImportanceMethod.html#method-FeatureImportanceMethod-reset'><code>xplainfi::FeatureImportanceMethod$reset()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PerturbationImportance-new"></a>}}
\if{latex}{\out{\hypertarget{method-PerturbationImportance-new}{}}}
\subsection{Method \code{new()}}{
Creates a new instance of the PerturbationImportance class
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PerturbationImportance$new(
  task,
  learner,
  measure,
  resampling = NULL,
  features = NULL,
  sampler = NULL,
  relation = "difference",
  iters_perm = 1L
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{task, learner, measure, resampling, features}}{Passed to FeatureImportanceMethod}

\item{\code{sampler}}{(\link{FeatureSampler}) Sampler to use for feature perturbation}

\item{\code{relation}}{(character(1)) How to relate perturbed scores to originals. Can be overridden in \verb{$compute()}.}

\item{\code{iters_perm}}{(integer(1)) Number of permutation iterations. Can be overridden in \verb{$compute()}.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PerturbationImportance-clone"></a>}}
\if{latex}{\out{\hypertarget{method-PerturbationImportance-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PerturbationImportance$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
