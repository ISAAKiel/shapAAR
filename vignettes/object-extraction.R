## ---- fig.cap = "Bell Beaker 1727 from @harrison_1977"-------------------
library(shapAAR)
library(EBImage)
my_image <- readImage("../inst/extdata/harrison_1977_1727.png")
display(my_image, method="raster")

## ----image_contrast_enhancement------------------------------------------
my_image <- normalize(my_image)^5
display(my_image, method="raster")

## ----image_resize--------------------------------------------------------
my_image <- resize(my_image, h = 500, antialias = T)
display(my_image, method="raster")

## ----image_canvas--------------------------------------------------------
my_image <- add_canvas(my_image,10,10,center = T)
display(my_image, method="raster")

## ----image_thresholding--------------------------------------------------
bw_image <- channel(my_image, "grey")
bw_image <- bw_image > otsu(bw_image)
display(bw_image, method="raster")

## ----image_smoothing-----------------------------------------------------
bw_image_smooth <- gblur(bw_image,1)
display(bw_image_smooth, method="raster")

## ----image_stopping_fun--------------------------------------------------
bw_image_smooth <- round(1 + bw_image_smooth * 255)
bw_image_smooth <- bw_image_smooth - mean(bw_image_smooth)
g <- stopping_fun(bw_image_smooth)
display(g, method="raster")

## ----active_contor_setup_parameter---------------------------------------
v <- 1
dt <- 1
phi <- default_phi(g)
n_iter <- 1000
buffer <- 2

## ----active_contor_setup_run---------------------------------------------
phi_out <- active_contour(phi, g, n_iter = n_iter, v=v, dt=dt, show = FALSE, buffer = buffer)
inner <- Image(phi_out<=0)
display(inner, method="raster")

## ----active_contor_result_compare----------------------------------------
segmented <- paintObjects(Image(inner),toRGB(bw_image), col='#ff00ff')
EBImage::display(segmented, method="raster", interpolate=F)

## ----active_contor_result_shrink-----------------------------------------
inner <- fillHull(inner)
 for (i in 1:buffer) {
   inner <- EBImage::erode(inner)
 }
segmented <- paintObjects(inner,toRGB(bw_image), col='#ff00ff')
EBImage::display(segmented, method="raster", interpolate=F)

## ----active_contor_object_selection--------------------------------------
labelled_img <- EBImage::bwlabel(inner)
features <- EBImage::computeFeatures.shape(labelled_img)
all_objects <- 1:max(labelled_img)
biggest_object <- which.max(features[,1])
img_biggest_only <- rmObjects(labelled_img,all_objects[all_objects!=biggest_object])
EBImage::display(normalize(img_biggest_only), method="raster", interpolate=F)

## ----active_contor_object_crop-------------------------------------------
img_biggest_only <- img_crop_background(img_biggest_only)
EBImage::display(img_biggest_only, method="raster", interpolate=F)

## ----active_contor_object_rotation, fig.show='hold'----------------------
EBImage::display(img_biggest_only, method="raster", interpolate=F)

fg_points <- which(img_biggest_only!=0,arr.ind = T)
minbbox <- getMinBBox(fg_points)
moment <- 90 -minbbox$angle
if (abs(moment)>45) {moment = moment - sign(moment)*90}
img_rect <- EBImage::rotate(img_biggest_only, moment, bg.col="black")
img_rect <- img_rect > otsu(img_rect)
img_rect <-img_crop_background(img_rect)

EBImage::display(img_rect, method="raster", interpolate=F)

## ----active_contor_object_rim_crop, fig.show='hold'----------------------
EBImage::display(img_rect, method="raster", interpolate=F)
img_crop <- crop_bottom(crop_top(img_rect))

EBImage::display(img_crop, method="raster", interpolate=F)

## ----active_contor_object_profile_image_split, fig.show='hold'-----------
dim_x <- nrow(img_crop)
splitaxis <- dim_x %/% 2

forward_ind <- 1:splitaxis
backward_ind <- sort((dim_x+1) - forward_ind)

left_img <- img_crop[forward_ind,]
right_img <- img_crop[backward_ind,]

EBImage::display(normalize(left_img), method="raster", interpolate=F)
EBImage::display(normalize(right_img), method="raster", interpolate=F)

## ----profile_image_split_overlay-----------------------------------------
left_img <- flop(left_img)

segmented <- paintObjects(left_img,toRGB(right_img), col='#ff00ff')
EBImage::display(segmented, method="raster", interpolate=F)

## ----active_contor_object_profile_contour_deviance, fig.show='hold'------
c_left <- contour_vector_bg(left_img)
c_right <- contour_vector_bg(right_img)

deviance <- sd(c_left - c_right)/length(c_left)
thresh <- 0.02
deviance < thresh

## ----profile_contour_mean, fig.show='hold'-------------------------------
c_mean <- round(apply(cbind(c_left, c_right),1,mean))

mean_profile <- profile_to_image(c_mean,dim(left_img)[1])
left_profile <- profile_to_image(c_left,dim(left_img)[1])
right_profile  <- profile_to_image(c_right,dim(left_img)[1])

segmented <- paintObjects(left_profile,toRGB(flop(right_img)), col='#0000ff')
segmented <- paintObjects(right_profile,segmented, col='#00ff00')
segmented <- paintObjects(mean_profile,segmented, col='#ff0000')
EBImage::display(segmented, method="raster", interpolate=F)

EBImage::display(mean_profile, method="raster", interpolate=F)

## ----final_proof, fig.show='hold'----------------------------------------
full_profile <- EBImage::tile(EBImage::combine(mean_profile, flop(mean_profile)), nx=2, lwd=0)
full_profile <- EBImage::rotate(full_profile,angle = -1 * moment)
full_profile <- resize(full_profile, w = nrow(full_profile), h = ncol(full_profile), output.dim = dim(bw_image))
full_profile <- EBImage::translate(full_profile,c(47,14))
#EBImage::display(bw_image, method="raster", interpolate=F)
segmented <- paintObjects(full_profile,toRGB(bw_image), col='#ff0000')
EBImage::display(segmented, method="raster", interpolate=F)


