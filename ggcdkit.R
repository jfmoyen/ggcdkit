library(tidyverse)
 
 # library(ggtern) # For now, ggtern breaks ggplot themes...
 
 # annotate is a function that exists both in GCDkit and in ggplot
 # GCDkit is not very careful about that, so we load it last to erase the original (ggplot) annotate
 # Use ggplot annotation layers with EXTREME CARE !

# library(conflicted)
# annotate <- GCDkitDevelop::annotate
## Equivalent to:
GCDkitDevelop::.assignWithNamespaceHard("annotate",GCDkitDevelop::annotate)


theme_gcdkit <- function () { 
  theme_bw(base_size=12, base_family="Avenir") %+replace% 
    theme(
      panel.background  = element_blank(),
      plot.background = element_blank(), 
      legend.background = element_rect(fill="transparent", colour=NA),
      legend.key = element_rect(fill="transparent", colour=NA),
      panel.grid = element_blank(),
      axis.title.x = element_text(size=14, face="bold", colour = "black"),
      axis.title.y = element_text(size=14, face="bold", colour = "black",angle=90),
      axis.text.x = element_text(size=12, colour = "black"),
      axis.text.y = element_text(size=12, colour = "black",angle=90),
      axis.ticks.length = unit(.25, "cm")
    )
}

GCDkitToTibble <- function(gcdWR=WR,gcdlabels=labels){
  #' Bind GCDkit WR and labels into a single tibble
  sampleNames <- rownames(gcdWR)
  DS <- as_tibble(gcdWR) %>% 
    mutate(ID=sampleNames, .before=1 ) %>%
    bind_cols(gcdlabels)
  
  return(DS)
}

##### Binary plot, gg version #####
ggbinary <- function(data, x, y) {

  pointsize = par("ps") # see notes in ggplotDiagram
  point_size_magic_nbr <- pointsize/5
  
  
  # The function can accept x and y both as strings (quoted) or unquoted
  # non-parseable varnames (i.e. with space or operators) must be back-ticked `like this`

  if(class(try(x,silent=T))=="try-error"){
    # X was supplied unquoted
    # This is fine for aes mapping (although we must use quasiquotation to prevent errors)
    # But we must retrieve the literal
    x <- enquo(x)
    xlab <- GCDkitDevelop::annotate(deparse(x) %>% str_replace("~","") %>% str_replace_all("`",""))
  }else{
    # We have the literal, must eval it for aes()
    xlab <- GCDkitDevelop::annotate(x %>% str_replace_all("`",""))
    x<-rlang::parse_expr(x)
  }
  
  if(class(try(y,silent=T))=="try-error"){
    y <- enquo(y)
    ylab <- GCDkitDevelop::annotate(deparse(y) %>% str_replace("~","") %>% str_replace_all("`","") )
  }else{
    ylab <- GCDkitDevelop::annotate(y %>% str_replace_all("`",""))
    y<-rlang::parse_expr(y)
  }
  

  ## Now we have x and y as quosures and xlab and ylab as (formatted) strings. All good ! 

 pl<- ggplot(data) + 
    geom_point(aes(!!x, !!y,colour=Colour,shape=Symbol,size=ds$Size*point_size_magic_nbr))+
    scale_shape_identity()+
    scale_color_identity()+
    scale_size_identity()+
    labs(x=xlab,y=ylab)+
    theme_gcdkit()
 
 print(pl)
 
 # We return (invisibly) the graphical object, which then allows to further manipulate it
 invisible(pl)
}


##########################
#
#  Capture a Figaro template and make it a ggplot
#
##########################
# TODO - keep the original dataset to be able to access other mappings (facets..)

# Level 1 : the main function
# Level 2 : the switch function that makes the right elements
# Level 3 : a series of converter functions


##### Converter functions #####

#### Lines ####
# Lines understand the following aesthetics:
# x y col lty lwd pch
# pch is not implemented

makeLineElement <- function(tpl_el){
  #' Convert a figaro line template element into a ggplot layer
  #' @param tpl_el: the figaro template element (which is a list with several elements)

# the_line <- tibble(x=tpl_el$x,y=tpl_el$y)
# gg_el <- geom_path(data=the_line,aes(x,y,colour=tpl_el$col,linetype=tpl_el$lty, size=tpl_el$lwd))

  # We need sensible defaults
  if(is.null(tpl_el$col)){col<-"black"}else{col<-tpl_el$col}
  if(is.null(tpl_el$lty)){lty<-"solid"}else{lty<-tpl_el$lty}
  if(is.null(tpl_el$lwd)){lwd<-1}else{lwd<-tpl_el$lwd}
  
  gg_el <- ggplot2::annotate(geom="path",x=tpl_el$x, y=tpl_el$y,
                             colour=col,linetype=lty, size=lwd)
  return(gg_el)
}

#### Polygon ####
# Polygon understand the following aesthetics:
# x y col border
# border is set to FALSE in figaro, so we just use color= NA
makePolygonElement <- function(tpl_el){
  #' Convert a figaro polygon template element into a ggplot layer
  #' @param tpl_el: the figaro template element (which is a list with several elements)

  the_poly <- tibble(x=tpl_el$x,y=tpl_el$y)
  gg_el <- geom_polygon(data=the_poly,aes(x,y,fill=tpl_el$col,color=NA))
  
  return(gg_el)
}

#### Arrows ####
# Arrows understand the following aesthetics:
# x0 y0 x1 y1 code col angle length lty lwd
makeArrowsElement <- function(tpl_el){ 
  #' Convert a figaro arrows template element into a ggplot layer
  #' @param tpl_el: the figaro template element (which is a list with several elements)

    # Convert the numeric code of base::arrows() to the ends arg of grid::arrow()
    ends <- switch(tpl_el$code,
                   "first",
                   "last",
                   "both")
    
    gg_el <- geom_segment(aes(x=tpl_el$x0,y=tpl_el$y0,
                              xend=tpl_el$x1,yend=tpl_el$y1,
                              color=tpl_el$col,linetype=tpl_el$lty, size=tpl_el$lwd),
                              arrow=arrow(angle=tpl_el$angle,length=unit(tpl_el$length,"inches"),ends=ends))
    
    return(gg_el)
    
  }

#### Text ####
# Text understand the following aesthetics:
# x y text col cex font adj srt pos
makeTextElement <- function(tpl_el){ 
  #' Convert a figaro text template element into a ggplot layer
  #' @param tpl_el: the figaro template element (which is a list with several elements)

  # Carry the variables from the parent function
  text_size_magic_nbr <- dynGet("text_size_magic_nbr")
  
  # There is no reliable default for these, so if they are not supplied we must define them...
  if(is.null(tpl_el$srt)){angle<-0}else{angle<-tpl_el$srt} 
  if(is.null(tpl_el$cex)){cex<-1}else{cex<-tpl_el$cex} 
  if(is.null(tpl_el$col)){col<-"black"}else{col<-tpl_el$col} 
  
  # adj is a bit more complex, as ggplot does not take a vector but wants 2 scalars
  # also pos can override adj in grapics::text
  vadj <- NA
  if(is.null(tpl_el$adj)){
    hadj <- 0.5
  }else{
    hadj<-tpl_el$adj[1]
    if(length(tpl_el$adj==2)){
      vadj <- tpl_el$adj[2]
    }
  } 
  
  # Override adj if pos is called (help(graphics::text))
  if(!is.null(tpl_el$pos)){
    if(tpl_el$pos==1){hadj=0.5;vadj=1} # below
    if(tpl_el$pos==2){hadj=0;vadj=0.5} # left
    if(tpl_el$pos==3){hadj=0.5;vadj=0} # above
    if(tpl_el$pos==4){hadj=1;vadj=0.5} # right
  }
  
  # Convert the graphics::text font definitions to ggplot compatible
  if(is.null(tpl_el$font)){the_font<-"plain"}else{
    the_font<- switch(tpl_el$font,
                      "plain",
                      "bold",
                      "italic",
                      "bold.italic")
  }
  
  # Build the text label (annotation layer)
  gg_el <- ggplot2::annotate("text",label=tpl_el$text,x=tpl_el$x,y=tpl_el$y,
                             colour=col,
                             size=cex*text_size_magic_nbr,
                             angle=angle,
                             hjust = hadj,
                             vjust = vadj,
                             fontface = the_font)

  return(gg_el)
  }

#### abline ####
# abline understand the following aesthetics:
# a b h v col lty lwd
makeAblineElement <- function(tpl_el){  
  #' Convert a figaro abline template element into a ggplot layer
  #' @param tpl_el: the figaro template element (which is a list with several elements)

  # We need sensible defaults
  if(is.null(tpl_el$col)){col<-"black"}else{col<-tpl_el$col}
  if(is.null(tpl_el$lty)){lty<-"solid"}else{lty<-tpl_el$lty}
  if(is.null(tpl_el$lwd)){lwd<-1}else{lwd<-tpl_el$lwd}
  
  if(!is.null(tpl_el$v)){
    ## vline is "a special geometry that does not work as annotation" (duh)
    # gg_el <- ggplot2::annotate(geom="abline",xintercept = tpl_el$v,
    #                            colour=col,linetype=lty,size=lwd)
gg_el <- NULL # for now
    # gg_el <- geom_vline(aes(xintercept = tpl_el$v,colour=tpl_el$col,
    #                         linetype=tpl_el$lty,size=tpl_el$lwd))
  }

  if(!is.null(tpl_el$h)){
    gg_el <- geom_hline(aes(yintercept = tpl_el$h,colour=tpl_el$col,
                            linetype=tpl_el$lty,size=tpl_el$lwd))
  }
  
  if(!is.null(tpl_el$a)){
    gg_el <- geom_abline(aes(intercept = tpl_el$a,slope=tpl_el$b,
                             colour=tpl_el$col,
                             linetype=tpl_el$lty,size=tpl_el$lwd))
  }
  
  return(gg_el)
}

#### Points ####
# points understand the following aesthetics:
# x y col pch cex
makePointsElement <- function(tpl_el){   
  #' Convert a figaro points template element into a ggplot layer
  #' @param tpl_el: the figaro template element (which is a list with several elements)

  # Carry the variables from the parent function
  point_size_magic_nbr <- dynGet("point_size_magic_nbr")
  
  the_pts <- tibble(x=tpl_el$x,y=tpl_el$y)
  
  if(is.null(tpl_el$cex)){cex<-1}else{cex<-tpl_el$cex}
  
  gg_el <- geom_point(data=the_pts,aes(x,y,
                                       colour=tpl_el$col,
                                       shape=tpl_el$pch,
                                       size=cex*point_size_magic_nbr))
  
  return(gg_el)
}

#### Curve ####
# curve understand the following aesthetics:
# equation col lwd lty
# At present time no diagram uses this...
makeCurveElement <- function(tpl_el){ 
  #' Convert a figaro curve template element into a ggplot layer
  #' @param tpl_el: the figaro template element (which is a list with several elements)

  gg_el <- geom_function(fun = as.expression(equation),
                             colour=tpl_el$col,linetype=tpl_el$lty, size=tpl_el$lwd)
  
  return(gg_el)
}

#### Reservoirs ####
# reservoirs understand the following aesthetics:
# well, not quite... but they are called like thuis
# reservoirs=figAddReservoirs(autoscale=FALSE,var.name=args$var.name,sample.names=args$sample.names,
#                             reserv.condition=args$reserv.condition,
#                             labs = args$labs,col=args$col,pch=args$pch,
#                             cex=args$cex,type=args$type,just.draw=TRUE),# added by VJ

makeReservoirsElement <- function(tpl_el){ 
  #' Convert a figaro reservoirs template element into a ggplot layer
  #' @param tpl_el: the figaro template element (which is a list with several elements)

  
  # Carry the variables from the parent function
  point_size_magic_nbr <- dynGet("point_size_magic_nbr")
  text_size_magic_nbr <- dynGet("text_size_magic_nbr")
  
  x.lab <- dynGet("x.lab")
  y.lab <- dynGet("y.lab")

  plot_type <- dynGet("plot_type")

    # Only possible for binaries, yet.
  if(plot_type!="binary"){
    cat("Cannot add reservoirs on",plot_type,"\n")
    return(NULL)
  }
  
    # the_res<-parseReservoirs(tpl_el$var.name)
  
  # Get the right reservoir table (rely on GCDkit routines to parse the format)
  all_res_m <- switch(tpl_el$var.name, 
                      reservoirs.data = .figAddReservoirsLoad(), 
                      idealmins.data = .figAddReservoirsMinsLoad(), 
                      debon.ideal.data = .figAddReservoirsDebonLoad(), 
                      get(var.name, .GlobalEnv) ) 
  
  # Convert it to tibble (to be able to pass it to ggplot)
  # resNames <- rownames(all_res_m)
  # all_res <- all_res_m %>% as_tibble() %>%
  #   mutate(resName=resNames, .before=1 )
  
  # Now we must extract the right values...
  # Cleanup
  .purgeAxName <- function(w.lab){
    # Extracted from GCDkit functions to sanitize names...
    ww <- .fig.deeval(w.lab)
    ww <- gsub("[ ][+][ ]", "+", ww)
    ww<- gsub("([[:alpha:]]+)([ ]?)([=]+)([ ]?)([[:print:]]+)",
         "\\1", ww)
    return(ww)
  }
  
  # do it...
  xx <- .purgeAxName(x.lab)
  yy <- .purgeAxName(y.lab)
  
 # Get the coordinates
  xval<- calcCore(xx,"all_res_m",redo = F)$results
  yval<- calcCore(yy,"all_res_m",redo = F)$results
  
  the_res <- data.frame(xval,yval)
  
  # Filter
  
  if(!is.null(tpl_el$sample.names)){ #######
    the_res <- the_res[tpl_el$sample.names,]
  }
  
  if(!is.null(tpl_el$reserv.condition)){
    ii<-str_detect(rownames(the_res),tpl_el$reserv.condition)
    the_res <- the_res[ii,]
  }
  
  # Size
  if(is.null(tpl_el$cex)){cex<-1}else{cex<-tpl_el$cex}
 
  # Add them to the plot
  gg_el <- list(geom_point(data=the_res,aes(x=xval,y=yval,
                                            colour=tpl_el$col,
                                            shape=tpl_el$pch,
                                            size=cex*point_size_magic_nbr)),
                ggplot2::annotate("text",label=tpl_el$labs,
                                  x=the_res$xval,y=the_res$yval,
                                  colour=tpl_el$col,
                                  size=cex*text_size_magic_nbr*0.75,
                                  hjust = 0.5,
                                  vjust = -0.5,
                                  fontface = "bold")
  )
 
  tt <- tpl_el[-1]
  if(!is.null(tt$type)&&tt$type=="l"){ 
    gg_el <- c(gg_el,
               geom_line(data=the_res,aes(x=xval,y=yval,
                                          colour=tpl_el$col)))
  }
  
  return(gg_el)
}

##### Switch function #####
make_templ_element<-function(tpl_el){
  #' Convert a figaro template element into a ggplot layer
  #' This function is mostly a switch function that calls the relevant component
  #' @param tpl_el: the figaro template element (which is a list with several elements)

  # The sub-functions expect to find, in various places of the parent env:
  # text_size_magic_nbr <- dynGet("text_size_magic_nbr")
  # point_size_magic_nbr <- dynGet("point_size_magic_nbr")
  # 
  # x.lab <- dynGet("x.lab")
  # y.lab <- dynGet("y.lab")
  
  # Get the type of the element
  # it can be one of lines, polygon, arrows, text, abline, points, curve, reservoir
  # not implemented: etext, box, rug, axis, ArcGIS, phasePlot, PTmap, legend, mtext
  names(tpl_el)[1]<-"type"

  # Initialize
  gg_el <- NULL
  
  # Lines
  if(tpl_el$type=="lines"&&!is.null(tpl_el$col)){ # A line without colour does not exist...
    gg_el <- makeLineElement(tpl_el)
  }
  
  # Polygon
  if(tpl_el$type=="polygon"){ 
    gg_el <- makePolygonElement(tpl_el)
  }
  
  # Arrows
  if(tpl_el$type=="arrows"){ 
    gg_el <- makeArrowsElement(tpl_el)
  }
 
  # Text
  if(tpl_el$type=="text"){ 
    gg_el <- makeTextElement(tpl_el)
  }
  
  # abline
  if(tpl_el$type=="abline"){ 
    gg_el <- makeAblineElement(tpl_el)
  }

  # Points
  if(tpl_el$type=="points"){ 
    gg_el <- makePointsElement(tpl_el)
  }
  
  # curve
  if(tpl_el$type=="curve"){ 
    cat("Trying to parse curve from template - EXPERIMENTAL\n")
    try(gg_el <- makeCurveElement(tpl_el))
  }
  
  # reservoirs
  #### TODO there are many cases and subcases with reservoirs
  # Perhaps, consider moving it out of the main function and write ggAddReservoirs, or something
  # and add it manually rather than risking to break everything
  # The real difficulty here are the Debon cationic parameters that do not have the same names 
  # in diagram templates and in debon.idealmins.data
  # Probably easier to rewrite the template and the idealmins to align them...
  # Also they don't work on ternary...
   if(tpl_el$type=="reservoirs"){ 
    cat("Trying to parse reservoir from template - EXPERIMENTAL\n")
    try(gg_el <- makeReservoirsElement(tpl_el))
 }

    # Finally !
  return(gg_el)
}

#### Main function ####
ggplotDiagram<-function(diagram,plot=T,...){
  
  if(class(diagram)=="character"){
    do.call(diagram,list(...)) 
  }else{
    diagram(...)
  }
  
  plot_type <- sheet$demo$template$GCDkit$plot.type
  
  xlims <- sheet$demo$call$xlim
  ylims <- sheet$demo$call$ylim
  
  x.lab <- sheet$demo$call$xlab
  y.lab <- sheet$demo$call$ylab
  
  ## X and Y scale (log or natural)
  
  # Default
  scale_x <- scale_x_continuous(expand=c(0,0))
  scale_y <- scale_y_continuous(expand=c(0,0))

  if(!is.null(sheet$demo$call$log)){
    if(sheet$demo$call$log=="x"||sheet$demo$call$log=="xy" ){
      scale_x <- scale_x_log10(expand=c(0,0))
    }
    
    if(sheet$demo$call$log=="y"||sheet$demo$call$log=="xy" ){
      scale_y <- scale_y_log10(expand=c(0,0))
    }
  }

  # Build the plotting dataset
  # Keep all the original info, plus add the calculated coordinates - when they are calculated

  # Gather x and y data
  if(is.null(x.data)||is.null(y.data)||length(x.data)<=1||length(y.data)<=1){
    # Not a very nice solution (drop=F would be better) but meanwhile...
    cat('Nothing happened - not enough data to plot diagram !\n')
    return('not enough data to plot diagram !')
  }
  
  xtibble <- tibble(ID=names(x.data),x.data=x.data)
  ytibble <- tibble(ID=names(y.data),y.data=y.data)

    # Convert GCDkit data to tibble format
    plottingDS <- GCDkitToTibble() %>%
      left_join(xtibble,by="ID") %>%
      left_join(ytibble,by="ID") 

  template <- sheet$demo$template
 
  pointsize = par("ps") # the default R pointsize: this is the number of pts corresponding to a size 1 text/symbol (cex=1)
  
  # in ggplot the sizes are given in mm, with the conversion factor in .pt
  # So the adjustment factor for text is
  text_size_magic_nbr <- pointsize/.pt
  
  # ggplot has default symbol size of 5 mm (see GeomPoint$default_aes ? ) compared to <pointsize> pts in basic R
  # So to emulate the graphs we must account for that
  point_size_magic_nbr <- pointsize/5
  
  # The defaut size of the text is theme_get()$text$size
 
  plt<- ggplot(plottingDS) + 
    geom_point(aes(x=x.data, y=y.data,colour=Colour,shape=Symbol,size=Size*point_size_magic_nbr))+
    map(template,make_templ_element)+
    scale_shape_identity()+
    scale_color_identity()+
    scale_fill_identity()+
    scale_linetype_identity()+
    scale_size_identity()+
    scale_x+
    scale_y+
    coord_cartesian(xlim=xlims,ylim=ylims)+
    labs(x=x.lab,y=y.lab)+
    theme_gcdkit()
  
  # Suppress the "real" axes for a ternary
  
  if(plot_type == "ternary"){
    plt<-plt+theme(axis.line = element_blank(),
                   panel.border = element_blank(),
                   axis.ticks = element_blank(),
                   axis.title = element_blank(),
                   axis.text.x = element_blank(),
                   axis.text.y = element_blank(),
                   aspect.ratio = 1)
  }
  
  if(plot){print(plt)}
  invisible(plt)
}



################### EXPERIMENTAL CODE ##############################

##### Ternary
# ggtern(ds)+ 
#   geom_point(aes(x=Al2O3,y=MgO,z=FeOt,colour=Colour,shape=Symbol))+
#   scale_shape_identity()+
#   scale_color_identity()+
#   theme_gcdkit()+
#   tern_limits(T=0.5,R=0.6,L=0.9)

##### Spider
# chdr <- selectNorm("Anders & Grevesse 1989")
# pm <- selectNorm("REE Primitive mantle (McDonough & Sun 1995)")
# 
# chdr %<>% as_tibble(rownames = "Reservoir") %>% mutate(Reservoir = "chondrite")
# pm %<>% as_tibble(rownames = "Reservoir") %>% mutate(Reservoir = "Prim.Mantle")
# 
# df <- df[,c("Reservoir",REE)] %>% add_row(chdr[,c("Reservoir",REE)]) %>% add_row(pm[,c("Reservoir",REE)])
# 
# 
# df %>% 
#   filter(Reservoir == "chondrite" | Reservoir == "Prim.Mantle" | Reservoir == "DMM") %>%
#   mutate(across(where(is.numeric), ~./.[Reservoir == "chondrite"])) %>%
#   gather(REE,key="Element",value=Concentration) %>% 
#   ggplot(aes(x=Element,y=Concentration,col=Reservoir,group=Reservoir))+
#   geom_point(size=3)+
#   xlim(REE)+
#   labs(y="Concentration normalisée",title="Normalisé aux chondrites")+
#   scale_y_log10()+
#   geom_path(size=1)+
#   theme_gcdkit()+theme(panel.grid.major=element_line(colour="grey",linetype="dashed"))

######## Capture template in its own environment (not in global)
# Perhaps useful in reactive context...
# library(ggplot2)
# library(magrittr)
# library(tibble)
# my.better.plot <- function(){
#   x.coords <- 1
#   y.coords <- 1
#   environment(package.plot) <- environment()
#   
#   bmp(tempfile())
#   package.plot()
#   dev.off()
#   
#   print(tibble(x.coords,y.coords) %>% ggplot(aes(x=x.coords,y=y.coords))+geom_point()) # etc. 
# }
# 
# my.better.plot()
# #creates only the ggplot in the current device
# 
# ls(globalenv())
# #[1] "my.better.plot" "package.plot"   "the.data" 



################### DEAD CODE ##############################

########## Parse reservoirs.data ##################
## Can be done with GCDkit function

#' parseReservoirs <- function(resfile="reservoirs.data"){
#'   #' A function to convert the reservoirs.data/spider.data format into a tibble
#'   #' @param resfile: the filename (starting from GCDkit root directory)
#'   
#'   # Scan the text file and get a list
#'   normal <- scan(paste(gcdx.dir, resfile, 
#'                        sep = "/"), skip = 1, comment.char = "#", 
#'                  what = list(mname = "", elements = "", nvalues = ""), 
#'                  sep = "\n", quiet = TRUE)
#'   
#'   # Transpose the list
#'   normal %>% transpose -> foo
#'   
#'   # For each element, we convert it into a tibble of elements/compositions
#'   normToTibble<-function(normBlock){
#'     tfoo<-tibble(name=normBlock$mname,
#'                  elements = unlist(strsplit(normBlock$elements,",")),
#'                  values = as.numeric(unlist(strsplit(normBlock$nvalues,","))))
#'     
#'     return(tfoo %>% pivot_wider(names_from=elements,values_from=values))
#'   }
#'   
#'   # Map/reduce
#'   return( map(foo,normToTibble) %>% reduce(.f=bind_rows) )
#' }


############# Simpler version of previous - extract template

# ggGetFigTemplate<-function(diagram){
#   
#   ## works only if GCDkit data is loaded. If not we need dummy data...
#   ### Temp solution - but not robust with millicats, WRanh and others....
#   ### Also not robust if missing elements
#   ### Need a complete collection of dummies
#   if(!exists("WR")){
#     data(sazava)
#     accessVar("sazava")
#   }
#   ### 
#   
#   diagram()
#   
#   plot_type <- sheet$demo$template$GCDkit$plot.type
#   
#   xlims <- sheet$demo$call$xlim
#   ylims <- sheet$demo$call$ylim
#   
#   xlab <- sheet$demo$call$xlab
#   ylab <- sheet$demo$call$ylab
#   
#   plottingDS <- tibble(x=x.data,y=y.data,col=labels$Colour,sym=labels$Symbol,Size=labels$Size)
#   
#   template <- sheet$demo$template
# 
#   pointsize = par("ps") # the default R pointsize: this is the number of pts corresponding to a size 1 text/symbol (cex=1)
#   
#   # in ggplot the sizes are given in mm, with the conversion factor in .pt
#   # So the adjustment factor for text is
#   text_size_magic_nbr <- pointsize/.pt
#   
#   # ggplot has default symbol size of 5 mm (see GeomPoint$default_aes ? ) compared to <pointsize> pts in basic R
#   # So to emulate the graphs we must account for that
#   point_size_magic_nbr <- pointsize/5
#   
#   make_templ_element<-function(tpl_el){
#     #browser()
#     
#     names(tpl_el)[1]<-"type"
#     # cat("plotting ",tpl_el$type,"\n")
#     gg_el <- NULL
#     # Lines
#     if(tpl_el$type=="lines"&&!is.null(tpl_el$col)){ # A line without colour does not exist...
#       the_line <- tibble(x=tpl_el$x,y=tpl_el$y)
#       gg_el <- geom_path(data=the_line,aes(x,y,colour=tpl_el$col,linetype=tpl_el$lty))
#     }
#     
#     # Ablines
#     if(tpl_el$type=="abline"){ 
#       # print(tpl_el)
#       if(!is.null(tpl_el$v)){
#         gg_el <- geom_vline(aes(xintercept = tpl_el$v,colour=tpl_el$col,linetype=tpl_el$lty))
#       }
#       if(!is.null(tpl_el$h)){
#         gg_el <- geom_hline(aes(yintercept = tpl_el$h,colour=tpl_el$col,linetype=tpl_el$lty))
#       }
#       
#     }
#     
#     # Text
#     if(tpl_el$type=="text"){ 
#       
#       # There is no reliable default for these, so if they are not supplied we must define them...
#       if(is.null(tpl_el$srt)){angle<-0}else{angle<-tpl_el$srt} 
#       if(is.null(tpl_el$cex)){cex<-1}else{cex<-tpl_el$cex} 
#       if(is.null(tpl_el$col)){col<-"black"}else{col<-tpl_el$col} 
#       
#       # adj is a bit more complex, as ggplot does not take a vector but wants 2 scalars
#       vadj <- NA
#       if(is.null(tpl_el$adj)){
#         hadj <- 0.5
#       }else{
#         hadj<-tpl_el$adj[1]
#         if(length(tpl_el$adj==2)){
#           vadj <- tpl_el$adj[2]
#         }
#       } 
#       
#       # Build the text label (annotation layer)
#       gg_el <- ggplot2::annotate("text",label=tpl_el$text,x=tpl_el$x,y=tpl_el$y,
#                                  colour=col,
#                                  size=cex*text_size_magic_nbr,
#                                  angle=angle,
#                                  hjust = hadj)
#     }
#     
#     
#     return(gg_el)
#     
#   }
# 
#    return(map(template,make_templ_element))
#    
# }
# 
