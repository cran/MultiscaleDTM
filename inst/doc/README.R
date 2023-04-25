## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = TRUE, warning = FALSE, message = FALSE)

## ----setup, include=FALSE-----------------------------------------------------
md_fig_dir<- "../man/figures/" #Path relative to this Rmd
R_fig_dir<- "../figures/" #Path relative to child Rmd

## ----message=FALSE------------------------------------------------------------
library(MultiscaleDTM) #Load MultiscaleDTM package

## ----eval=FALSE---------------------------------------------------------------
#  help(package="MultiscaleDTM")

## -----------------------------------------------------------------------------
r<- erupt()

## ----Topo, echo= FALSE, message=FALSE-----------------------------------------
library(tmap) #For plotting
tm_shape(r, raster.downsample = FALSE)+
  tm_raster(palette = colorRamps::matlab.like(100), style = "cont", legend.reverse = TRUE, title = "")+
  tm_layout(legend.outside=TRUE, main.title= "Elevation")

## -----------------------------------------------------------------------------
slp_asp<- SlpAsp(r = r, w = c(5,5), unit = "degrees", method = "queen", metrics = c("slope", "aspect", "eastness", "northness"), na.rm=TRUE)

## ----SlpAsp, echo=FALSE-------------------------------------------------------
slp_asp_list<- vector(mode="list", length = nlyr(slp_asp))
for (i in 1:length(slp_asp_list)) {
  curr_var<- names(slp_asp)[i]
  
  if (grepl(pattern = "(^northness)|(^eastness)", curr_var)) {
    breaks<- c(-1,0,1)
    midpoint<- 0
    curr_pal<- c("blue", "gray", "red")
  } else if (grepl(pattern = "aspect", curr_var)) {
    curr_pal<-c("blue", "purple", "red", "orange", "yellow", "green", "cyan", "blue")
    breaks<- c(0,90,180,270,360)
    midpoint<- 180
  } else{
    curr_pal<- colorRamps::matlab.like(100)
    midpoint<- NULL
    breaks<- NULL
  }
  
  slp_asp_list[[i]]<- tm_shape(slp_asp[[i]], raster.downsample = FALSE) +
    tm_raster(palette = curr_pal, style= "cont", title = "", breaks = breaks, midpoint = midpoint, legend.reverse = TRUE)+
      tm_layout(main.title = curr_var, 
      main.title.position = "center",
      main.title.size=0.75)
  }
slp_asp_plot<- tmap_arrange(slp_asp_list, ncol=2)
slp_asp_plot

## -----------------------------------------------------------------------------
qmetrics<- Qfit(r, w = c(5,5), unit = "degrees", metrics = c("elev", "qslope", "qaspect", "qeastness", "qnorthness", "profc", "planc", "twistc", "meanc", "maxc", "minc", "features"), na.rm = TRUE)

## ----Qfit, echo= FALSE--------------------------------------------------------
library(cowplot)
for (i in 1:nlyr(qmetrics)) {
  curr_var<- names(qmetrics)[i]
  i_txt<- as.character(i)
  if(nchar(i)==1){i_txt<- paste0("0", i_txt)}
  
  if (grepl(pattern = "(northness)|(eastness)", curr_var)) {
    breaks<- c(-1,0,1)
    midpoint<- 0
    curr_pal<- c("blue", "gray", "red")
    style<- "cont"
  } else if (grepl(pattern = "aspect", curr_var)) {
    curr_pal<-c("blue", "purple", "red", "orange", "yellow", "green", "cyan", "blue")
    breaks<- c(0,90,180,270,360)
    midpoint<- 180
    style<- "cont"
    } else if(grepl(pattern = "^features", curr_var)) {
    curr_pal<- c("gray", "orange", "black", "blue", "green", "yellow", "red")
  } else if(grepl(pattern = "c$", curr_var)){
    curr_pal<- c("blue", "gray", "red")
    style<- "cont"
    breaks<- NULL
    midpoint<- 0
  } else{
    curr_pal<- colorRamps::matlab.like(100)
    midpoint<- NULL
    breaks<- NULL
    style<- "cont"}
  
  if(!grepl(pattern = "^features", curr_var)){
    qfit_plt<- tm_shape(qmetrics[[i]], raster.downsample = FALSE) +
    tm_raster(palette = curr_pal, style= style, title = "", breaks = breaks, midpoint = midpoint, legend.reverse = TRUE)+
      tm_layout(legend.outside = TRUE, legend.text.size = 0.7, legend.frame=FALSE,
      legend.outside.size = 0.4, outer.margins = 0.01, asp = 0, frame = FALSE)
    tmap_save(qfit_plt, filename = paste0(R_fig_dir, "sub_qfit", i_txt, ".png"), dpi=150, width =3, height = 2, units = "in")
    } else{
      png(filename = paste0(R_fig_dir, "sub_qfit", i_txt, ".png"), res = 150, width =3.5, height = 3, units = "in")
      qfit_plt<- plot(qmetrics[[i]],col=curr_pal, axes=FALSE, box=FALSE, cex=0.1)
      dev.off()
      } 
}

qfit_plt_files<- list.files(R_fig_dir, pattern = "sub_qfit\\d{2}.png$", full.names = TRUE)
qfit_plt_list<- vector(mode = "list", length = length(qfit_plt_files))
for (i in seq_along(qfit_plt_list)) {
  qfit_plt_list[[i]]<- ggdraw()+ draw_image(qfit_plt_files[[i]])
  }
save_plot(plot_grid(plotlist = qfit_plt_list, ncol = 3, labels = names(qmetrics), label_size = 12, align = "hv"),filename= paste0(R_fig_dir, "qmetrics.jpg"), base_width=7.5, base_height=9, dpi= 150, unit="in")

## -----------------------------------------------------------------------------
vrm<- VRM(r, w=c(5,5), na.rm = TRUE)

## ----VRM, echo=FALSE----------------------------------------------------------
tm_shape(vrm, raster.downsample = FALSE)+
  tm_raster(palette = colorRamps::matlab.like(100), style = "cont", legend.reverse = TRUE, title="")+
  tm_layout(legend.outside = TRUE, main.title="VRM")

## -----------------------------------------------------------------------------
sapa<- SAPA(r, w=c(5,5), slope_correction = TRUE)

## ----SAPA, echo=FALSE---------------------------------------------------------
tm_shape(sapa, raster.downsample = FALSE)+
  tm_raster(palette = colorRamps::matlab.like(100), style = "cont", legend.reverse = TRUE, title="")+
  tm_layout(legend.outside = TRUE, main.title="SAPA")

## -----------------------------------------------------------------------------
adj_SD<- AdjSD(r, w=c(5,5), na.rm = TRUE)

## ----AdjSD, echo=FALSE--------------------------------------------------------
tm_shape(adj_SD, raster.downsample = FALSE)+
  tm_raster(palette = colorRamps::matlab.like(100), style = "cont", legend.reverse = TRUE, title="")+
  tm_layout(legend.outside = TRUE, main.title="Adjusted SD")

## -----------------------------------------------------------------------------
rie<- RIE(r, w=c(5,5), na.rm = TRUE)

## ----RIE, echo=FALSE----------------------------------------------------------
tm_shape(rie, raster.downsample = FALSE)+
  tm_raster(palette = colorRamps::matlab.like(100), style = "cont", legend.reverse = TRUE, title="")+
  tm_layout(legend.outside = TRUE, main.title="Roughness Index-Elevation")

## -----------------------------------------------------------------------------
rp<- RelPos(r, w=matrix(data = c(1,NA,1), nrow = 3, ncol=3), shape = "custom", fun = "median", na.rm = TRUE)

## ----RP, echo=FALSE-----------------------------------------------------------
tm_shape(rp, raster.downsample = FALSE)+
  tm_raster(palette = c("blue", "gray", "red"), style = "cont", midpoint=0, legend.reverse = TRUE, title="")+
  tm_layout(legend.outside = TRUE, main.title="Relative Position")

## -----------------------------------------------------------------------------
tpi<- TPI(r, w=c(5,5), shape= "rectangle", na.rm = TRUE)

## ----TPI, echo=FALSE----------------------------------------------------------
tm_shape(tpi, raster.downsample = FALSE)+
  tm_raster(palette = c("blue", "gray", "red"), style = "cont", midpoint=0, legend.reverse = TRUE, title="")+
  tm_layout(legend.outside = TRUE, main.title="TPI")

## -----------------------------------------------------------------------------
dmv<- DMV(r, w=2, shape= "circle", na.rm = TRUE, stand="range")

## ----DMV, echo=FALSE----------------------------------------------------------
tm_shape(dmv, raster.downsample = FALSE)+
  tm_raster(palette = c("blue", "gray", "red"), style = "cont", midpoint=0, legend.reverse = TRUE, title="")+
  tm_layout(legend.outside = TRUE, main.title="sDMV")

## -----------------------------------------------------------------------------
bpi<- BPI(r, w = c(4,6), unit = "cell", stand= "sd", na.rm = TRUE)

## ----BPI, echo=FALSE----------------------------------------------------------
tm_shape(bpi, raster.downsample = FALSE)+
  tm_raster(palette = c("blue", "gray", "red"), style = "cont", midpoint=0, legend.reverse = TRUE, title="")+
  tm_layout(legend.outside = TRUE, main.title="sBPI")

## -----------------------------------------------------------------------------
annulus_window(radius = c(4,6), unit = "cell")

## -----------------------------------------------------------------------------
bpi2<- BPI(r, w = annulus_window(radius = c(4,6), unit = "cell"), stand= "sd", na.rm = TRUE) # equivalent to BPI code from earlier

