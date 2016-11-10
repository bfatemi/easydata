

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