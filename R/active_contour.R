#' Evolves an active contour
#'
#' @param phi an inital contour
#' @param g the stopping function of the contour
#' @param n_iter the maximal number of iterations
#' @param v the velocity of the development of the contour
#' @param dt the time interval of the contour development
#' @param buffer the buffer of the contour around the shape
#' @param show should the development of the contour be shown interactively
#' @param orig_image the original image on which the contour should be developed. Only necessary if \code{show=TRUE}
#' @return the active contour around the features of the image
#'
#' @export

active_contour <- function(phi,
                           g,
                           n_iter = 2000,
                           v = 1,
                           dt = 1,
                           buffer = 3,
                           show = FALSE,
                           orig_image = NA) {
    dg <- grad(phi)
    phi_before <- phi
    for (i in 1:n_iter) {
        dphi <- grad(phi)

        dphi_norm <- norm(dphi)

        kappa <- curvature(phi)

        smoothing <- g * kappa * dphi_norm

        balloon <- g * dphi_norm * v

        attachment <- dot(dphi, dg)

        dphi_t <- smoothing + balloon + attachment
        phi <- phi + dt * dphi_t

        if (i %% 4 == 0) {
            sign_phi <- sign(phi)
            phi <- (distmap(sign_phi + 1) - distmap(sign_phi - 1)) - buffer
            if (abs(sum( (phi <= 0) - (phi_before <= 0)) ) == 0) {
                break
            }
            phi_before <- phi
        }
        if (show && i %% 10 == 0) {
            segmented <- paintObjects(Image(phi <= 0),
                                      toRGB(orig_image), col = "#ff00ff")
            EBImage::display(segmented, method = "raster", interpolate = F)
        }
    }
    return(phi)
}