simulator <- function(nline=5, npoint=30, nnoise=100, p_min=-10, p_max=10, coef_min=-5, coef_max=5,
                      noise_sd=0.1, slope_sd=1, coordinate='cartesian',
                      line_noise=TRUE, noise=TRUE, show=TRUE){
  # '''
  # nline: number of lines
  # npoint: number of points on each line
  # nnoise: number of noise point background
  # p_min, p_max: range of random generated number from 0
  # coef_min, coef_max: range of random generated coefficient from 0
  # b_min, b_max: range of random generated intercept from 0
  # noise_sd: range of of random generated number for noise from 0
  # coordinate: cartesian or polar
  # line_noise: add noise to points on line or not
  # noise: perfect lines ot lines with noise
  # show: show plots or not
  # '''

  x_coords = c()
  y_coords = c()

  for(i in 1:nline){

    # noise coefficient
    if(line_noise==TRUE){
      e = rnorm(npoint, mean=0, sd=noise_sd)
    } else if (line_noise==FALSE){
      e = 0
    }

    if(coordinate=='polar'){ # polar coefficient

      t = runif(npoint, 0, 180) * pi / 180
      x = runif(1, p_min, p_max)
      y = runif(1, p_min, p_max)
      # r = b/(x * cos(t) + y * sin(t))
      r = x * cos(t) + y * sin(t) + e

      x_coords = c(x_coords, t)
      y_coords = c(y_coords, r)

    } else if(coordinate=='cartesian'){ # cartesian

      x = runif(npoint, p_min, p_max)
      m = rnorm(1, mean=0, sd=slope_sd)
      b = runif(1, p_min, p_max)
      y = m*x + b + e

      x_coords = c(x_coords, x)
      y_coords = c(y_coords, y)

    } else if (coordinate=='circle') {
      s = runif(npoint, 0, 2*pi)
      #m = runif(1, p_min, p_max)
      m = 5
      b1 = runif(1, coef_min, coef_max)
      b2 = runif(1, coef_min, coef_max)
      sinx = m*sin(s) + b1
      cosy = m*cos(s) + b2

      x_coords = c(x_coords, sinx)
      y_coords = c(y_coords, cosy)
    }
  }

  if(noise==FALSE | nnoise<=0){
    noises = data.frame()
  } else {
    random_x <- runif(nnoise, p_min, p_max)
    random_y <- runif(nnoise, p_min, p_max)

    x_coords = c(x_coords, random_x)
    y_coords = c(y_coords, random_y)
  }

  coord = data.frame(x=x_coords, y=y_coords)

  if(show==TRUE){plot(coord$x, coord$y)}

  return(coord)
}
