# .onLoad <- function(libname, pkgname) {
#   register_s3_method("ggplot2", "autoplot", "bumbldf", gen_pkg = "ggplot2")
# }
#
# register_s3_method <- function(pkg, generic, class, fun = NULL, gen_pkg = pkg) {
#   stopifnot(is.character(pkg), length(pkg) == 1)
#   stopifnot(is.character(generic), length(generic) == 1)
#   stopifnot(is.character(class), length(class) == 1)
#   if (is.null(fun)) {
#     fun <- get(paste0(generic, ".", class), envir = parent.frame())
#   }
#   stopifnot(is.function(fun))
#
#   if (pkg %in% loadedNamespaces()) {
#     envir <- asNamespace(gen_pkg)
#     registerS3method(generic, class, fun, envir = envir)
#   }
#
#   # Always register hook in case package is later unloaded & reloaded
#   setHook(
#     packageEvent(pkg, "onLoad"),
#     function(...) {
#       envir <- asNamespace(gen_pkg)
#       registerS3method(generic, class, fun, envir = envir)
#     }
#   )
# }


.onLoad <- function(libname, pkgname) {
  vctrs::s3_register("ggplot2::autoplot", "bumbldf")
}
