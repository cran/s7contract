## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
knitr::read_chunk(
  system.file("examples", "vector-laws.R", package = "s7contract")
)

## ----setup--------------------------------------------------------------------
library(S7)
library(s7contract)

## ----vector-interface---------------------------------------------------------
vec_length <- new_generic("vec_length", "x")
vec_slice <- new_generic("vec_slice", "x", function(x, i) S7_dispatch())
vec_values <- new_generic("vec_values", "x")

VectorLike <- new_interface(
  "VectorLike",
  generics = list(
    length = interface_requirement(vec_length, returns = class_integer),
    slice = interface_requirement(vec_slice, args = list(i = class_integer)),
    values = interface_requirement(vec_values, returns = class_double)
  )
)

ReadDepth <- new_class(
  "ReadDepth",
  properties = list(position = class_integer, depth = class_double),
  validator = function(self) {
    if (length(self@position) != length(self@depth)) {
      "@position and @depth must have the same length"
    }
  }
)

method(vec_length, ReadDepth) <- function(x) length(x@depth)
method(vec_slice, ReadDepth) <- function(x, i) {
  ReadDepth(position = x@position[i], depth = x@depth[i])
}
method(vec_values, ReadDepth) <- function(x) x@depth

method(vec_length, class_double) <- function(x) length(x)
method(vec_slice, class_double) <- function(x, i) x[i]
method(vec_values, class_double) <- function(x) x

coverage <- ReadDepth(position = 1:5, depth = c(12, 15, 9, 20, 17))
implements(coverage, VectorLike)
implements(class_double, VectorLike)

## ----vector-consumer----------------------------------------------------------
window_mean <- function(x, i) {
  assert_implements(x, VectorLike)
  with(VectorLike, mean(vec_values(vec_slice(x, i))))
}

window_mean(coverage, 2:4)
window_mean(c(12, 15, 9, 20, 17), 2:4)

## ----checked-indices----------------------------------------------------------
tryCatch(
  with(VectorLike, vec_slice(coverage, "first")),
  error = function(e) conditionMessage(e)
)

## ----measurement-trait--------------------------------------------------------
Measured <- new_trait("Measured",
  methods = list(values = trait_method(vec_values)),
  assoc_consts = "UNITS"
)
has_trait(ReadDepth, Measured)

impl_trait(Measured, ReadDepth,
  methods = list(values = function(x) x@depth),
  assoc_consts = list(UNITS = "reads"),
  replace = TRUE
)
has_trait(ReadDepth, Measured)
trait_assoc_const(Measured, ReadDepth, "UNITS")

## ----length-law---------------------------------------------------------------
length_law <- new_law("length matches constructor input",
  generators = list(values = gen_vector(gen_double(-10, 10), max = 6L)),
  holds = function(values) {
    x <- ReadDepth(position = seq_along(values), depth = values)
    with(VectorLike, identical(vec_length(x), base::length(values)))
  }
)
check_law(length_law, tests = 100L, seed = 1L)

