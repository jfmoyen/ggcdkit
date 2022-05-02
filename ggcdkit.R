

 library(tidyverse)
 
 # library(ggtern) # For now, ggtern breaks ggplot themes...
 
 # annotate is a function that exists both in GCDkit and in ggplot
 # GCDkit is not very careful about that, so we load it last to erase the original (ggplot) annotate
 # Use ggplot annotation layers with EXTREME CARE !
 library(GCDkitDevelop)



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

# ggtern(ds)+ 
#   geom_point(aes(x=Al2O3,y=MgO,z=FeOt,colour=Colour,shape=Symbol))+
#   scale_shape_identity()+
#   scale_color_identity()+
#   theme_gcdkit()+
#   tern_limits(T=0.5,R=0.6,L=0.9)

##########################
#
#  Capture a Figaro template and make it a ggplot
#
##########################


ggplotDiagram<-function(diagram){
  
  diagram()
  
  plot_type <- sheet$demo$template$GCDkit$plot.type
  
  xlims <- sheet$demo$call$xlim
  ylims <- sheet$demo$call$ylim
  
  xlab <- sheet$demo$call$xlab
  ylab <- sheet$demo$call$ylab
  
  plottingDS <- tibble(x=x.data,y=y.data,col=labels$Colour,sym=labels$Symbol,Size=labels$Size)
  
  template <- sheet$demo$template
  
  
  
  pointsize = par("ps") # the default R pointsize: this is the number of pts corresponding to a size 1 text/symbol (cex=1)
  
  # in ggplot the sizes are given in mm, with the conversion factor in .pt
  # So the adjustment factor for text is
  text_size_magic_nbr <- pointsize/.pt
  
  # ggplot has default symbol size of 5 mm (see GeomPoint$default_aes ? ) compared to <pointsize> pts in basic R
  # So to emulate the graphs we must account for that
  point_size_magic_nbr <- pointsize/5
  
  
  
  # The defaut size of the text is theme_get()$text$size
  
  make_templ_element<-function(tpl_el){
    #browser()
    
    names(tpl_el)[1]<-"type"
    # cat("plotting ",tpl_el$type,"\n")
    gg_el <- NULL
    # Lines
    if(tpl_el$type=="lines"&&!is.null(tpl_el$col)){ # A line without colour does not exist...
      the_line <- tibble(x=tpl_el$x,y=tpl_el$y)
      gg_el <- geom_path(data=the_line,aes(x,y,colour=tpl_el$col,linetype=tpl_el$lty))
    }
    
    # Ablines
    if(tpl_el$type=="abline"){ 
      # print(tpl_el)
      if(!is.null(tpl_el$v)){
        gg_el <- geom_vline(aes(xintercept = tpl_el$v,colour=tpl_el$col,linetype=tpl_el$lty))
      }
      if(!is.null(tpl_el$h)){
        gg_el <- geom_hline(aes(yintercept = tpl_el$h,colour=tpl_el$col,linetype=tpl_el$lty))
      }
      
    }
    
    # Text
    if(tpl_el$type=="text"){ 
      
      # There is no reliable default for these, so if they are not supplied we must define them...
      if(is.null(tpl_el$srt)){angle<-0}else{angle<-tpl_el$srt} 
      if(is.null(tpl_el$cex)){cex<-1}else{cex<-tpl_el$cex} 
      if(is.null(tpl_el$col)){col<-"black"}else{col<-tpl_el$col} 
      
      # adj is a bit more complex, as ggplot does not take a vector but wants 2 scalars
      vadj <- NA
      if(is.null(tpl_el$adj)){
        hadj <- 0.5
      }else{
        hadj<-tpl_el$adj[1]
        if(length(tpl_el$adj==2)){
          vadj <- tpl_el$adj[2]
        }
      } 
      
      # Build the text label (annotation layer)
      gg_el <- ggplot2::annotate("text",label=tpl_el$text,x=tpl_el$x,y=tpl_el$y,
                                 colour=col,
                                 size=cex*text_size_magic_nbr,
                                 angle=angle,
                                 hjust = hadj)
    }
    
    
    return(gg_el)
    
  }
  
  plt<- ggplot(plottingDS) + 
    geom_point(aes(x, y,colour=col,shape=sym,size=Size*point_size_magic_nbr))+
    map(template,make_templ_element)+
    scale_shape_identity()+
    scale_color_identity()+
    scale_linetype_identity()+
    scale_size_identity()+
    scale_x_continuous(expand=c(0,0))+
    scale_y_continuous(expand=c(0,0))+
    coord_cartesian(xlim=xlims,ylim=ylims)+
    labs(x=xlab,y=ylab)+
    theme_gcdkit()
  
  # Suppress the "real" axes for a ternary
  if(plot_type == "ternary"){
    plt<-plt+theme(axis.line = element_blank(),
                   panel.border = element_blank(),
                   axis.ticks = element_blank(),
                   axis.title = element_blank(),
                   axis.text.x = element_blank(),
                   axis.text.y = element_blank())
  }
  
  print(plt)
  invisible(plt)
}

############# Simpler version of previous - extract template

ggGetFigTemplate<-function(diagram){
  
  ## works only if GCDkit data is loaded. If not we need dummy data...
  ### Temp solution - but not robust with millicats, WRanh and others....
  ### Also not robust if missing elements
  ### Need a complete collection of dummies
  if(!exists("WR")){
    data(sazava)
    accessVar("sazava")
  }
  ### 
  
  diagram()
  
  plot_type <- sheet$demo$template$GCDkit$plot.type
  
  xlims <- sheet$demo$call$xlim
  ylims <- sheet$demo$call$ylim
  
  xlab <- sheet$demo$call$xlab
  ylab <- sheet$demo$call$ylab
  
  plottingDS <- tibble(x=x.data,y=y.data,col=labels$Colour,sym=labels$Symbol,Size=labels$Size)
  
  template <- sheet$demo$template

  pointsize = par("ps") # the default R pointsize: this is the number of pts corresponding to a size 1 text/symbol (cex=1)
  
  # in ggplot the sizes are given in mm, with the conversion factor in .pt
  # So the adjustment factor for text is
  text_size_magic_nbr <- pointsize/.pt
  
  # ggplot has default symbol size of 5 mm (see GeomPoint$default_aes ? ) compared to <pointsize> pts in basic R
  # So to emulate the graphs we must account for that
  point_size_magic_nbr <- pointsize/5
  
  make_templ_element<-function(tpl_el){
    #browser()
    
    names(tpl_el)[1]<-"type"
    # cat("plotting ",tpl_el$type,"\n")
    gg_el <- NULL
    # Lines
    if(tpl_el$type=="lines"&&!is.null(tpl_el$col)){ # A line without colour does not exist...
      the_line <- tibble(x=tpl_el$x,y=tpl_el$y)
      gg_el <- geom_path(data=the_line,aes(x,y,colour=tpl_el$col,linetype=tpl_el$lty))
    }
    
    # Ablines
    if(tpl_el$type=="abline"){ 
      # print(tpl_el)
      if(!is.null(tpl_el$v)){
        gg_el <- geom_vline(aes(xintercept = tpl_el$v,colour=tpl_el$col,linetype=tpl_el$lty))
      }
      if(!is.null(tpl_el$h)){
        gg_el <- geom_hline(aes(yintercept = tpl_el$h,colour=tpl_el$col,linetype=tpl_el$lty))
      }
      
    }
    
    # Text
    if(tpl_el$type=="text"){ 
      
      # There is no reliable default for these, so if they are not supplied we must define them...
      if(is.null(tpl_el$srt)){angle<-0}else{angle<-tpl_el$srt} 
      if(is.null(tpl_el$cex)){cex<-1}else{cex<-tpl_el$cex} 
      if(is.null(tpl_el$col)){col<-"black"}else{col<-tpl_el$col} 
      
      # adj is a bit more complex, as ggplot does not take a vector but wants 2 scalars
      vadj <- NA
      if(is.null(tpl_el$adj)){
        hadj <- 0.5
      }else{
        hadj<-tpl_el$adj[1]
        if(length(tpl_el$adj==2)){
          vadj <- tpl_el$adj[2]
        }
      } 
      
      # Build the text label (annotation layer)
      gg_el <- ggplot2::annotate("text",label=tpl_el$text,x=tpl_el$x,y=tpl_el$y,
                                 colour=col,
                                 size=cex*text_size_magic_nbr,
                                 angle=angle,
                                 hjust = hadj)
    }
    
    
    return(gg_el)
    
  }

   return(map(template,make_templ_element))
   
}


