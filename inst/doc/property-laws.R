## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----setup--------------------------------------------------------------------
library(S7)
library(s7contract)
tinytest::using(s7contract)

## ----reverse-law--------------------------------------------------------------
reverse_law <- new_law(
  "reverse is involutive",
  generators = list(
    x = gen_vector(gen_integer(-100L, 100L), max = 20L)
  ),
  holds = function(x) identical(rev(rev(x)), x)
)

expect_law(reverse_law, tests = 100L, seed = 20260902L)

## ----interface-law------------------------------------------------------------
Circle <- new_class(
  "CirclePropertyVignette",
  properties = list(radius = class_double),
  validator = function(self) {
    if (self@radius < 0) "`radius` must be non-negative."
  }
)
area <- new_generic(
  "area_property_vignette",
  "x",
  function(x) S7_dispatch()
)
method(area, Circle) <- function(x) pi * x@radius^2

HasArea <- new_interface(
  "HasAreaPropertyVignette",
  generics = list(
    area = interface_requirement(area, returns = class_double)
  )
)
circles <- gen_map(
  gen_double(0, 1000),
  function(radius) Circle(radius = radius)
)
area_law <- new_law(
  "non-negative radii have non-negative area",
  generators = list(x = circles),
  holds = function(x) with(HasArea, area(x)) >= 0
)

expect_law(area_law, tests = 100L, seed = 20260902L)

## ----counterexample-----------------------------------------------------------
ten <- new_generator(
  draw = function(size) 10L,
  shrink = function(value) {
    if (value == 0L) list() else list(0L, value %/% 2L)
  },
  label = "ten",
  prototype = integer()
)
negative_law <- new_law(
  "generated values are negative",
  generators = list(x = ten),
  holds = function(x) x < 0L
)

failure <- check_law(negative_law, tests = 10L, seed = 20260902L)
failure

## ----replay-------------------------------------------------------------------
replayed <- do.call(check_law, c(list(law = failure@law), failure@parameters))
identical(replayed@counterexample@minimal, failure@counterexample@minimal)

## ----nested-vectors-----------------------------------------------------------
nested <- new_law(
  "nested vectors retain their element type",
  generators = list(x = gen_vector(gen_vector(gen_integer(), max = 4L), max = 3L)),
  holds = function(x) is.list(x) && all(vapply(x, is.integer, logical(1)))
)
expect_law(nested, tests = 20L, seed = 1L)

## ----dependent-sequences------------------------------------------------------
sequences <- gen_bind(gen_integer(0L, 20L), function(n) {
  gen_product(
    length = gen_constant(n),
    bases = gen_vector(gen_element(c("A", "C", "G", "T")), min = n, max = n)
  )
})
sequence_law <- new_law(
  "sequence length matches its declaration",
  generators = list(x = sequences),
  holds = function(x) length(x$bases) == x$length
)
expect_law(sequence_law, tests = 40L, seed = 1L)
gen_example(sequences, size = 10L, seed = 42L)

## ----nullable-values----------------------------------------------------------
nullable <- gen_choice(gen_constant(NA_integer_), gen_integer(), prob = c(1, 9))
gen_example(gen_vector(nullable, min = 6L, max = 6L), size = 10L, seed = 42L)

## ----recursive-values---------------------------------------------------------
trees <- gen_recursive(
  gen_element(c("A", "C", "G", "T")),
  function(child) gen_product(left = child, right = child)
)
gen_example(trees, size = 7L, seed = 42L)

leaf_count <- function(tree) {
  if (is.list(tree)) leaf_count(tree$left) + leaf_count(tree$right) else 1L
}
tree_law <- new_law(
  "binary trees have at least one leaf",
  generators = list(tree = trees),
  holds = function(tree) leaf_count(tree) >= 1L
)
expect_law(tree_law, tests = 30L, seed = 1L, max_size = 7L)

