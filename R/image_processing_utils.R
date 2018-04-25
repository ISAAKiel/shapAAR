#' Add canvas to an image
#'
#' Add canvas to an image of class \code{EBImage}.
#' @param x the original image, either of class \code{EBImage::Image} or \code{matrix}.
#' @param nrows,ncols The number of rows and columns of which the image should be enlarged
#' @param center should the original picture be centered within the canvas?
#' @param col the color of the canvas
#' @return The image with an enlarged canvas in
#' @export

add_canvas <- function(x, nrows, ncols, center = T, col = "white") {
    w <- dim(x)[1]
    h <- dim(x)[2]
    out <- EBImage::resize(x = x,
                           w = w,
                           h = h,
                           output.dim = c(w + nrows, h + ncols),
                           antialias = FALSE, bg.col = col)
    if (center) {
        out <- EBImage::translate(out,
                                  c(nrows %/% 2, ncols %/% 2),
                                  bg.col = col)
    }
}

#' Crop the image to content
#'
#' Crop the an image of class \code{EBImage} to only those pixels that are not background
#' @param x the image
#' @param bg the background value
#' @return the cropped image
#' @export

img_crop_background <- function(x, bg = 0) {
    x_range <- range(which(apply(x, 1, function(x) {
        !(all(x == bg))
    })))
    y_range <- range(which(apply(x, 2, function(x) {
        !(all(x == bg))
    })))
    cropped_img <- x[x_range[1]:x_range[2], y_range[1]:y_range[2]]
    cropped_img
}

#' Minimum-Area Bounding Box For A Set Of 2D-Points
#'
#' Calculates the vertices of the minimum-area, possibly oriented bounding box given a set of 2D-coordinates. Adapted from shotGroups package.
#' @param xy either a numerical (n x 2)-matrix with the (x,y)-coordinates of n >= 2 points (1 row of coordinates per point), or a data frame with either the variables \code{x}, \code{y} or \code{point.x}, \code{point.y}.
#' @return A list with the following information about the minimum-area bounding box:
#' \item{pts}{a (4 x 2)-matrix containing the coordinates of the (ordered) vertices.}
#' \item{width}{width of the box.}
#' \item{height}{height of the box.}
#' \item{FoM}{figure of merit, i.e., the average side length of the box: (\code{width} + \code{height}) / 2.}
#' \item{diag}{length of box diagonal.}
#' \item{angle}{orientation of the box' longer edge pointing up as returned by \code{\link{atan2}}, but in degree.}
#' @importFrom grDevices chull
#' @export

getMinBBox <- function(xy) {
    stopifnot(is.matrix(xy), is.numeric(xy), nrow(xy) >= 2, ncol(xy) == 2)

    # rotating calipers algorithm using the convex hull
    H <- chull(xy)  ## hull indices, vertices ordered clockwise
    n <- length(H)  ## number of hull vertices
    hull <- xy[H, ]  ## hull vertices

    # unit basis vectors for all subspaces spanned by the hull edges
    h_dir <- diff(rbind(hull, hull[1, ]))  ## hull vertices are circular
    h_lens <- sqrt(rowSums(h_dir ^ 2))  ## length of basis vectors
    hu_dir <- diag(1 / h_lens) %*% h_dir  ## scaled to unit length

    # unit basis vectors for the orthogonal subspaces rotation
    # by 90 deg -> y' = x, x' = -y
    ou_dir <- cbind(-hu_dir[, 2], hu_dir[, 1])

    # project hull vertices on the subspaces spanned by the hull edges,
    # and on the subspaces spanned by their orthogonal complements
    # - in subspace coords
    proj_mat <- rbind(hu_dir, ou_dir) %*% t(hull)

    # range of projections and corresponding width/height of bounding rectangle
    range_h <- matrix(numeric(n * 2), ncol = 2)  ## hull edge
    range_o <- matrix(numeric(n * 2), ncol = 2)  ## orthogonal subspace
    widths <- numeric(n)
    heights <- numeric(n)

    for (i in seq(along = numeric(n))) {
        range_h[i, ] <- range(proj_mat[i, ])

        # the orthogonal subspace is in the 2nd half of the matrix
        range_o[i, ] <- range(proj_mat[n + i, ])
        widths[i] <- abs(diff(range_h[i, ]))
        heights[i] <- abs(diff(range_o[i, ]))
    }

    # extreme projections for min-area rect in subspace coordinates hull edge
    # leading to minimum-area
    e_min <- which.min(widths * heights)
    h_proj <- rbind(range_h[e_min, ], 0)
    o_proj <- rbind(0, range_o[e_min, ])

    # move projections to rectangle corners
    h_pts <- sweep(h_proj, 1, o_proj[, 1], "+")
    o_pts <- sweep(h_proj, 1, o_proj[, 2], "+")

    # corners in standard coordinates, rows = x,y, columns = corners
    # in combined (4x2)-matrix: reverse point order to be usable in
    # polygon() basis formed by hull edge and orthogonal subspace
    basis <- cbind(hu_dir[e_min, ], ou_dir[e_min, ])
    h_corn <- basis %*% h_pts
    o_corn <- basis %*% o_pts
    pts <- t(cbind(h_corn, o_corn[, c(2, 1)]))

    # angle of longer edge pointing up
    d_pts <- diff(pts)
    e <- d_pts[which.max(rowSums(d_pts ^ 2)), ]  ## one of the longer edges
    e_up <- e * sign(e[2])  ## rotate upwards 180 deg if necessary
    deg <- atan2(e_up[2], e_up[1]) * 180 / pi  ## angle in degrees

    return(list(pts = pts,
                width = widths[e_min],
                height = heights[e_min],
                angle = deg))
}
