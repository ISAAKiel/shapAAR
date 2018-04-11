#' Get the profile contour by the first foreground pixel
#'
#' @param x the image of the profile of the vessel
#' @return a vector with the position of the profile
#' @export

contour_vector_fg <- function(x) {
    x <- add_canvas(x, 1, 0)
    apply(x, 2, function(x) {
        min(which(x == 0)) - 1
    })
}

#' Get the profile contour by the last background pixel
#'
#' @param x the image of the profile of the vessel
#' @return a vector with the position of the profile
#' @export

contour_vector_bg <- function(x) {
    x <- x[nrow(x):1, ]
    x <- add_canvas(x, 1, 0, col = "black")
    all_black <- apply(x,2,function(z) !any(z==1))
    out <- integer(length = ncol(x))
    out[!all_black] <- apply(x[,!all_black], 2, function(x) {
        min(which(x != 0)) - 1
    })
    out[all_black] <- nrow(x)
    return(out)
}
#' Turns a contour into an image again
#'
#' @param x the contour vector
#' @param columns_out the width of the resulting image
#' @return an image representing the profile contour
#' @export

profile_to_image <- function(x, columns_out) {
    profile_matrix <- sapply(seq(along = x), function(l) {
        c(rep(1, x[l]), rep(0, columns_out - x[l]))
    })
    profil_img <- Image(profile_matrix)
    profil_img
}
