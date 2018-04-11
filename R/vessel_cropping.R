#' Crop the image of a ceramic vessels to its lowest corner points
#'
#' @param x the image of a vessel
#' @return the vessel image cropped
#' @export

crop_bottom <- function(x) {
    bottom_line <- min(get_bottom_points(x))
    x <- x[, 1:bottom_line]
    return(x)
}

#' Crop the image of a ceramic vessels to its top corner points
#'
#' @param x the image of a vessel
#' @return the vessel image cropped
#' @export

crop_top <- function(x) {
    top_line <- max(get_top_points(x))
    x <- x[, top_line:ncol(x)]
    return(x)
}

#' Get the bottom break points of a ceramic vessels
#'
#' @param x the image of a vessel
#' @return the two lowest break point of the vessel bottom
#' @export

get_bottom_points <- function(x) {
    q_l_l <- flip(flop(x[1:(nrow(x) %/% 2), (ncol(x) %/% 2):ncol(x)]))
    p_l <- ncol(x) - get_corner(q_l_l)

    q_l_r <- flip(x[(nrow(x) %/% 2):nrow(x), (ncol(x) %/% 2):ncol(x)])
    p_r <- ncol(x) - get_corner(q_l_r)

    return(c(p_l, p_r))
}

#' Get the top break points of a ceramic vessels
#'
#' @param x the image of a vessel
#' @return the two break point of the vessel top
#' @export

get_top_points <- function(x) {
    q_u_l <- flop(x[1:(nrow(x) %/% 2), 1:(ncol(x) %/% 2)])
    p_l <- get_corner(q_u_l)

    q_u_r <- x[(nrow(x) %/% 2):nrow(x), 1:(ncol(x) %/% 2)]
    p_r <- get_corner(q_u_r)

    return(c(p_l, p_r))
}

#' Returns the break point of a vessel contour from one of its quandrants
#'
#' @param x the image of a vessel quadrant
#' @return the break point of that vessel quadrant
#' @export
get_corner <- function(x) {
    contour <- contour_vector_bg(x)
    contour <- contour[contour <= contour[1]]

    c_points <- cbind(contour, 1:(length(contour)))

    ind <- chull(c_points)
    c_points <- c_points[ind, ]
    ind_first_point <- which(c_points[, 2] == 1)
    c_points <- c_points[ind_first_point:nrow(c_points), ]

    d_c_points <- diff(c_points[chull(c_points), ])

    angle <- atan(d_c_points[, 2] / d_c_points[, 1]) * 180 / pi
    out <- c_points[min(which(abs(angle) > 10)), 2]
    return(out)
}
