# The following functions as well as the snake function is an R adaptation of a python script originally
# from Agustinus Kristiadi source: https://wiseodd.github.io/techblog/2016/11/20/levelset-segmentation/

#' Calculate the central difference (derivative) gradients (x,y) of a (image) matrix
#'
#' @param x a matrix
#' @return a list of two derivative matrices
#' @examples
#' my_matrix <- matrix(c(0,1,1,0), nrow=2)
#' grad(my_matrix)
#' @export

grad <- function(x) {
    return(list(t(grad1_c(t(x))), grad1_c(x)))
}

#' Calculate the euclidian norm of two matrices
#'
#' @param x a list of two matrices
#' @return the norm of those matrices
#' @examples
#' my_matrix <- matrix(c(0,1,1,0), nrow=2)
#' norm(grad(my_matrix))
#'@export

norm <- function(x) {
    sqrt(Reduce("+", lapply(x, function(x) {
        x ^ 2
    })))
}

#' A simple edge detection using the image derivatives
#'
#' @param x a matrix containing the intensity values (grayscale)
#' @return a matrix containing the edges
#' @export

simple_edge_detection <- function(x) {
    return(1 / (1 + norm(grad(x)) ^ 2))
}

#' Stopping function for the active contour algorithm
#'
#' The stopping function for the active contour algorithm,
#' in this implementation using the edges of the image.
#'
#' @param x a matrix containing the intensity values (grayscale)
#' @return the stopping function
#' @export

stopping_fun <- function(x) {
    simple_edge_detection(x)
}

#' Return the initial phi surface for active contour
#'
#' The function returns the initialize contour surface phi,
#' 5px from the image border. Values outside the curve are set to 1,
#' inside the curve to -1.
#'
#' @param x a matrix containing the values of the image
#' @return a matrix of the same dimensions
#' @export

default_phi <- function(x) {
    # i.e. 1 outside the curve, and -1 inside the curve
    dim_x <- dim(x)
    phi <- matrix(1, nrow = dim_x[1], ncol = dim_x[2])
    phi[5:(dim_x[1] - 5), 5:(dim_x[2] - 5)] <- -1
    return(phi)
}

#' Calculate the curvature of a active contour phi
#'
#' @param f the active contour function phi
#' @return the curvature of that function
#' @export

curvature <- function(f) {
    g_f <- grad(f)
    fy <- g_f[[1]]
    fx <- g_f[[2]]
    norm <- sqrt(fx ^ 2 + fy ^ 2) + 1e-08
    return(div(fx / norm, fy / norm))
}

#' Calculate the divergence of two gradient derivatives
#'
#' @param fx,fy the x and y gradient derivatives
#' @return the divergence of those gradients
#' @export

div <- function(fx, fy) {
    return(grad1_c(fx) + t(grad1_c(t(fy))))
}

#' Sum the Product of list of matrixes
#'
#' @param x,y two lists of matrices
#' @return the sum of the products
#' @export

dot <- function(x, y) {
    out <- lapply(seq_along(x), function(i) {
        x[[i]] * y[[i]]
    })
    Reduce("+", out)
}
