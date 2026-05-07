## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(S7)
library(s7contract)

## ----toy-container------------------------------------------------------------
MiniSummarizedExperiment <- new_class(
  "MiniSummarizedExperiment",
  properties = list(
    assays = class_list,
    row_data = class_data.frame,
    col_data = class_data.frame
  ),
  validator = function(self) {
    if (length(self@assays) == 0) {
      return("@assays must contain at least one matrix")
    }

    dims <- lapply(self@assays, dim)
    if (any(vapply(dims, is.null, logical(1)))) {
      return("every assay must be matrix-like")
    }

    first_dim <- dims[[1]]
    same_dim <- vapply(dims, identical, logical(1), first_dim)
    if (!all(same_dim)) {
      return("all assays must have the same dimensions")
    }

    if (nrow(self@row_data) != first_dim[[1]]) {
      return("@row_data must have one row per assay feature")
    }
    if (nrow(self@col_data) != first_dim[[2]]) {
      return("@col_data must have one row per assay sample")
    }
  }
)

counts <- matrix(
  c(10, 0, 3, 4, 12, 8),
  nrow = 3,
  dimnames = list(c("geneA", "geneB", "geneC"), c("sample1", "sample2"))
)

mini <- MiniSummarizedExperiment(
  assays = list(counts = counts, logcounts = log1p(counts)),
  row_data = data.frame(gc = c(0.42, 0.51, 0.37), row.names = rownames(counts)),
  col_data = data.frame(condition = c("control", "treated"), row.names = colnames(counts))
)

## ----assay-operations---------------------------------------------------------
assay_names <- new_generic("assay_names", "x")
feature_names <- new_generic("feature_names", "x")
sample_names <- new_generic("sample_names", "x")
assay_matrix <- new_generic("assay_matrix", "x")

method(assay_names, MiniSummarizedExperiment) <- function(x) names(x@assays)
method(feature_names, MiniSummarizedExperiment) <- function(x) rownames(x@assays[[1]])
method(sample_names, MiniSummarizedExperiment) <- function(x) colnames(x@assays[[1]])
method(assay_matrix, MiniSummarizedExperiment) <- function(x, name = assay_names(x)[[1]]) {
  x@assays[[name]]
}

assay_names(mini)
sample_names(mini)
assay_matrix(mini, "counts")[, "sample1"]

## ----assay-consumer-----------------------------------------------------------
LibrarySizeInput <- new_interface(
  "LibrarySizeInput",
  generics = list(assay_matrix = assay_matrix)
)

library_size <- function(x, assay = "counts") {
  assert_implements(x, LibrarySizeInput)
  mat <- assay_matrix(x, assay)
  colSums(mat)
}

implements(mini, LibrarySizeInput)
library_size(mini)

## ----assay-mock---------------------------------------------------------------
MockAssays <- new_class("MockAssays", properties = list(assays = class_list))

method(assay_matrix, MockAssays) <- function(x, name = "counts") {
  x@assays[[name]]
}

mock_counts <- matrix(
  c(1, 2, 3, 4),
  nrow = 2,
  dimnames = list(c("geneA", "geneB"), c("sampleA", "sampleB"))
)
mock <- MockAssays(assays = list(counts = mock_counts))

implements(mock, LibrarySizeInput)
library_size(mock)

## ----assay-trait, message = FALSE, warning = FALSE----------------------------
ExperimentLike <- new_trait(
  "ExperimentLike",
  methods = list(
    assay_names = trait_method(assay_names),
    feature_names = trait_method(feature_names),
    sample_names = trait_method(sample_names),
    assay_matrix = trait_method(assay_matrix)
  ),
  assoc_consts = c("ASSAY_ORIENTATION")
)

impl_trait(
  ExperimentLike,
  MiniSummarizedExperiment,
  methods = list(
    assay_names = function(x) names(x@assays),
    feature_names = function(x) rownames(x@assays[[1]]),
    sample_names = function(x) colnames(x@assays[[1]]),
    assay_matrix = function(x, name = assay_names(x)[[1]]) x@assays[[name]]
  ),
  assoc_consts = list(ASSAY_ORIENTATION = "features_by_samples"),
  replace = TRUE
)

has_trait(mini, ExperimentLike)
trait_assoc_const(ExperimentLike, mini, "ASSAY_ORIENTATION")

