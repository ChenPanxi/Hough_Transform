hough_space <- function(points, steps=180, thres=10, coordinate='cartesian', show=TRUE, nodup=TRUE){
  names(points) <- c("theta", "rho")

  if(coordinate=='cartesian'){
    theta_steps = seq(0, pi, by=pi/steps)
    thetas = rep(theta_steps, nrow(points))

    dat = cbind(x=points$theta,y=points$rho)
    tri_theta = matrix(c(cos(theta_steps), sin(theta_steps)), byrow=TRUE, nrow=2)
    rhos = t(dat %*% tri_theta)

    hough_curves = matrix(c(thetas, rhos), ncol=2)

    if(show==TRUE){plot(thetas, rhos, type='l')}

  } else if (coordinate=='circle'){

    p_steps = seq(0, 2*pi, pi/steps)
    radius = 5 # radius set to be 5

    circle_x = expand.grid(radius*sin(p_steps),points$theta)
    circle_y = expand.grid(radius*cos(p_steps),points$rho)

    circle_x = rowSums(circle_x)
    circle_y = rowSums(circle_y)

    hough_curves = matrix(c(circle_x, circle_y), ncol=2)

    if(show==TRUE){plot(circle_x, circle_y, pch='.', xlim=c(-15, 15), ylim=c(-15, 15))}
  }

  x_coord = hough_curves[,1]
  y_coord = hough_curves[,2]

  votes = as.data.frame(table(x_bin=cut(x_coord,breaks=steps), y_bin=cut(y_coord,breaks=steps)))
  idx = which(votes['Freq'] > thres, arr.ind=TRUE)
  freq_bin = votes[idx[,1],]

  itv_x = freq_bin[,'x_bin']
  end_x = unlist(strsplit(gsub("(?![,.-])[[:punct:]]", "", as.character(itv_x), perl=TRUE), ","))
  end_x = matrix(as.numeric(end_x), byrow=TRUE, ncol=2)
  coords_x = rowMeans(end_x)

  y_itv = freq_bin[,'y_bin']
  y_itv = unlist(strsplit(gsub("(?![,.-])[[:punct:]]", "", as.character(y_itv), perl=TRUE), ","))
  y_itv = matrix(as.numeric(y_itv), byrow=TRUE, ncol=2)
  coords_y = rowMeans(y_itv)

  hough_coords = data.frame(x=coords_x, y=coords_y)

  if(nodup==TRUE){
    nodup_idx = !duplicated(round(hough_coords), by=c("x", "y"))
    hough_coords = hough_coords[nodup_idx, ]
  }

  if(show==TRUE){points(hough_coords$x, hough_coords$y, pch=18, cex=2, col="red")}

  return(list(curve=hough_curves, coords=hough_coords))
}
