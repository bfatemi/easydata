

easy_ggtitle <- function(main, sub){
    ggtitle(expression(atop(main, atop(italic(sub), ""))))    
}

easytheme <- function(){
    theme(strip.text.x = element_text(face = "bold", size = 13),
          strip.text.y = element_text(face = "bold", size = 13, angle = 180),
          plot.title = element_text(size = 23, face = "bold", colour = "black", vjust = -1),
          axis.text.x = element_text(size = 12, face = "italic"),
          axis.title.x = element_text(size = 14, face = "italic", margin=margin(15,0,15,0)),
          axis.title.y = element_text(size = 14, face = "italic", margin=margin(0,15,0,15)))
}
easytitle <- function(main, sub){
    eval(ggtitle(substitute(atop(main, atop(italic(sub), "")))))
}

lmeqn <- function(m) {
    l <- list(a = format(coef(m)[1], digits = 2),
              b = format(abs(coef(m)[2]), digits = 2),
              r2 = format(summary(m)$r.squared, digits = 3))
    if (coef(m)[2] >= 0) {
        eq <- substitute(italic(y) == a+b*italic(log(x))*","~~italic(r)^2~"="~r2,l)
    }else{
        eq <- substitute(italic(y) == a - b*italic(log(x))*","~~italic(r)^2~"="~r2,l)
    }
    as.character(as.expression(eq))
}