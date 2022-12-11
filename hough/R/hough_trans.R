hough_trans <- function(points, points_hs, steps=30, coordinate='cartesian', show=TRUE){
  names(points) = c("x", "y")
  #names(points_hs) = c("x", "y")

  radius = 5
  if(coordinate=='circle'){

    x_df = expand.grid(x_center=hs_circle$coords$x, theta=seq(0, 2*pi, pi/steps))
    y_df = expand.grid(y_center=hs_circle$coords$y, theta=seq(0, 2*pi, pi/steps))
    xx = radius*sin(x_df$theta) + x_df$x_center
    yy = radius*cos(y_df$theta) + y_df$y_center
    params = data.frame(x=xx, y=yy)

    draw_circle <- function(center_coords){
      theta_seq = seq(0, 2*pi, pi/30)
      xx = radius*sin(theta_seq) + center_coords['x']
      yy = radius*cos(theta_seq) + center_coords['y']
      lines(xx, yy, lwd=3, xlim=c(-10, 10), ylim=c(-10, 10), col='red')
    }

    if(show==TRUE){
      plot(points$x, points$y, xlab='x', ylab='y')
      apply(points_hs$coords, 1, FUN=draw_circle)
    }

  } else if (coordinate=='cartesian'){
    x_coords = points_hs$coords$x
    y_coords = points_hs$coords$y

    slope = -cos(x_coords)/sin(x_coords)
    intercept = y_coords/sin(x_coords)

    params = data.frame(m=slope, b=intercept)

    if(show==TRUE){
      plot(points$x, points$y, xlab='x', ylab='y')
      apply(params, 1, function(a) {abline(a[2], a[1], lwd=3, col="red")})
    }
  }


  return(params)

}
