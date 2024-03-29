get_rxx <- function(mod, theta) {
  ti<-testinfo(mod, matrix(theta))
  rxx<- ti/(1+ti)
  return(data.frame(theta=theta,rxx=rxx))
}

plot_rxx <- function(mod,theta,thresh,title="Marginal Reliability") {
  # get reliabilities
  d <- get_rxx(mod,theta)
  
  # find the points where rxx is above threshold
  xx <- d[d$rxx >= thresh,]
  # grab the first and last rows as first and last points where rxx > threshold
  xx <- xx[c(1,nrow(xx)),]
  
  p <- ggplot(d, aes(x = theta, y = rxx)) +
    geom_line(linewidth = 1, alpha = .8) +
    geom_vline(xintercept = xx[1,'theta'], linetype = 5) +
    geom_vline(xintercept = xx[2,'theta'], linetype = 5) +
    geom_hline(yintercept = thresh, linetype = 2)+
    geom_label(data=xx, aes(x = theta, y = 0, label = round(theta,2)),label.size = NA, nudge_x = 0, nudge_y = .1) +
    labs(title = title,x = expression(theta), y = expression("r"[xx]) )+
    theme_classic(base_size = 12) + theme(plot.title = element_text(face="bold",hjust=.5))
  return(p)
}

## Usage:
# th <- seq(-5,5,.1)
# p <- rxx.plot(mod = my_mirt_model, theta = th, thresh = .8)
# p
