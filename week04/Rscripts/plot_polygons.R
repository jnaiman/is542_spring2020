draw_polygon = function(x, y, color="red"){
  xstart = min(x)
  xend = max(x)
  polygon(c(xstart,x,xend),c(0,y,0),col=color) # plots on top of previous plot
  
}