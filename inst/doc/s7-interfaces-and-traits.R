## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(S7)
library(s7contract)

## ----structural-interface-----------------------------------------------------
area <- new_generic("area", "x")
draw <- new_generic("draw", "x")

Circle <- new_class("Circle", properties = list(r = class_double))
Rect <- new_class("Rect", properties = list(w = class_double, h = class_double))

method(area, Circle) <- function(x) pi * x@r^2
method(draw, Circle) <- function(x) sprintf("circle(r = %s)", x@r)
method(area, Rect) <- function(x) x@w * x@h

Drawable <- new_interface("Drawable", generics = list(draw = draw))
Shape <- new_interface("Shape", generics = list(area = area), parents = Drawable)

implements(Circle, Shape)
implements(Rect, Shape)
missing_requirements(Rect, Shape)

## ----structural-consumer------------------------------------------------------
render <- function(x) {
  assert_implements(x, Drawable)
  draw(x)
}

render(Circle(r = 2))

## ----structural-mock----------------------------------------------------------
MockDrawable <- new_class("MockDrawable")
method(draw, MockDrawable) <- function(x) "mock drawing"

render(MockDrawable())

## ----dbi-like-class-family----------------------------------------------------
DatabaseConnection <- new_class("DatabaseConnection", abstract = TRUE)
MemoryConnection <- new_class(
  "MemoryConnection",
  parent = DatabaseConnection,
  properties = list(tables = class_list)
)

db_tables <- new_generic("db_tables", "con")
db_read_table <- new_generic(
  "db_read_table",
  "con",
  function(con, name) S7_dispatch()
)

method(db_tables, MemoryConnection) <- function(con) names(con@tables)
method(db_read_table, MemoryConnection) <- function(con, name) con@tables[[name]]

TableReader <- new_interface(
  "TableReader",
  generics = list(
    db_tables = interface_requirement(db_tables, returns = class_character),
    db_read_table = interface_requirement(
      db_read_table,
      args = list(name = class_character),
      returns = class_data.frame
    )
  )
)

first_table <- function(con) {
  assert_implements(con, TableReader)
  db_read_table(con, db_tables(con)[[1]])
}

con <- MemoryConnection(tables = list(iris = head(iris, 2)))
first_table(con)

## ----progressive-interface----------------------------------------------------
Canvas <- new_class("Canvas")

draw_on <- new_generic(
  "draw_on",
  c("x", "canvas"),
  function(x, canvas, position, ...) S7_dispatch()
)

method(draw_on, list(Circle, Canvas)) <- function(x, canvas, position, ...) {
  sprintf("circle(r = %s) at %s", x@r, position)
}

DrawableOnCanvas <- new_interface(
  "DrawableOnCanvas",
  generics = list(
    draw_on = interface_requirement(
      draw_on,
      args = list(canvas = Canvas, position = class_integer),
      returns = class_character
    )
  )
)

canvas <- Canvas()
circle <- Circle(r = 2)

implements(Circle, DrawableOnCanvas)
with(DrawableOnCanvas, draw_on(circle, canvas, position = 1L))
draw_on(circle, canvas, position = 1L) %::% DrawableOnCanvas

checked_draw <- with(DrawableOnCanvas, {
  function(x) draw_on(x, canvas, position = 1L)
})
checked_draw(circle)

## ----progressive-return-failure-----------------------------------------------
BadCircle <- new_class("BadCircle", properties = list(r = class_double))
method(draw_on, list(BadCircle, Canvas)) <- function(x, canvas, position, ...) {
  x@r
}

tryCatch(
  with(DrawableOnCanvas, draw_on(BadCircle(r = 2), canvas, position = 1L)),
  error = function(e) conditionMessage(e)
)

tryCatch(
  checked_draw(BadCircle(r = 2)),
  error = function(e) conditionMessage(e)
)

## ----number-interface---------------------------------------------------------
num_zero <- new_generic("num_zero", "x")
num_add <- new_generic("num_add", "x")
num_scale <- new_generic("num_scale", "x")

NumberLike <- new_interface(
  "NumberLike",
  generics = list(
    zero = num_zero,
    add = num_add,
    scale = num_scale
  )
)

method(num_zero, class_double) <- function(x) 0
method(num_add, class_double) <- function(x, y) x + y
method(num_scale, class_double) <- function(x, k) x * k

implements(class_double, NumberLike)
num_add(10, 5)
num_scale(10, 0.5)

## ----vector-interface---------------------------------------------------------
vec_length <- new_generic("vec_length", "x")
vec_slice <- new_generic("vec_slice", "x")
vec_values <- new_generic("vec_values", "x")

VectorLike <- new_interface(
  "VectorLike",
  generics = list(
    length = vec_length,
    slice = vec_slice,
    values = vec_values
  )
)

ReadDepth <- new_class(
  "ReadDepth",
  properties = list(
    position = class_integer,
    depth = class_double
  ),
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

coverage <- ReadDepth(
  position = 1:5,
  depth = c(12, 15, 9, 20, 17)
)

implements(coverage, VectorLike)
vec_values(vec_slice(coverage, 2:4))

## ----vector-consumer----------------------------------------------------------
window_mean <- function(x, i) {
  assert_implements(x, VectorLike)
  mean(vec_values(vec_slice(x, i)))
}

window_mean(coverage, 2:4)

## ----trait--------------------------------------------------------------------
perimeter <- new_generic("perimeter", "x")

Measurable <- new_trait(
  "Measurable",
  methods = list(
    area = trait_method(area),
    perimeter = trait_method(perimeter, default = function(x) NA_real_)
  ),
  assoc_consts = c("UNITS")
)

impl_trait(
  Measurable,
  Circle,
  methods = list(area = function(x) pi * x@r^2),
  assoc_consts = list(UNITS = "unitless"),
  replace = TRUE
)

has_trait(Circle, Measurable)
trait_call(Measurable, "area", Circle(r = 2))
trait_call(Measurable, "perimeter", Circle(r = 2))
trait_assoc_const(Measurable, Circle, "UNITS")

## ----monad-dictionary---------------------------------------------------------
Maybe <- new_class("Maybe", abstract = TRUE)
Nothing <- new_class("Nothing", parent = Maybe)
Just <- new_class("Just", parent = Maybe, properties = list(value = class_any))

MonadDict <- new_class(
  "MonadDict",
  properties = list(
    name = class_character,
    pure = class_function,
    bind = class_function
  )
)

dict_pure <- new_generic("dict_pure", "x")
dict_bind <- new_generic("dict_bind", "x")

MonadDictionary <- new_interface(
  "MonadDictionary",
  generics = list(
    pure = dict_pure,
    bind = dict_bind
  )
)

method(dict_pure, MonadDict) <- function(x, value) {
  (x@pure)(value)
}
method(dict_bind, MonadDict) <- function(x, mx, f) {
  (x@bind)(mx, f)
}

MaybeMonad <- MonadDict(
  name = "Maybe",
  pure = function(value) Just(value = value),
  bind = function(mx, f) {
    if (S7_inherits(mx, Nothing)) {
      Nothing()
    } else {
      f(mx@value)
    }
  }
)

implements(MaybeMonad, MonadDictionary)

## ----monad-dictionary-use-----------------------------------------------------
dict_bind(
  MaybeMonad,
  Just(value = 2),
  function(x) dict_pure(MaybeMonad, x + 1)
)

dict_bind(
  MaybeMonad,
  Nothing(),
  function(x) dict_pure(MaybeMonad, x + 1)
)

## ----monad-laws---------------------------------------------------------------
maybe_equal <- function(x, y) {
  if (S7_inherits(x, Nothing) && S7_inherits(y, Nothing)) {
    return(TRUE)
  }
  if (S7_inherits(x, Just) && S7_inherits(y, Just)) {
    return(identical(x@value, y@value))
  }
  FALSE
}

f <- function(x) dict_pure(MaybeMonad, x + 1)
g <- function(x) dict_pure(MaybeMonad, x * 2)
mx <- Just(value = 10)

c(
  left_identity = maybe_equal(
    dict_bind(MaybeMonad, dict_pure(MaybeMonad, 10), f),
    f(10)
  ),
  right_identity = maybe_equal(
    dict_bind(MaybeMonad, mx, function(x) dict_pure(MaybeMonad, x)),
    mx
  ),
  associativity = maybe_equal(
    dict_bind(MaybeMonad, dict_bind(MaybeMonad, mx, f), g),
    dict_bind(MaybeMonad, mx, function(x) dict_bind(MaybeMonad, f(x), g))
  )
)

