#  Cargamos las Librerias ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, ggplot2, ggspatial, colorspace, ggpubr, sf, elevatr, tmap, ggnewscale)

Ge            <- raster("Raster/tif1.tif")
Poligon  = st_read("SHP/piura.shp")
Poligo   <- st_transform(Poligon ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Geo<- projectRaster(Ge, crs = crs(Poligo))

elev = get_elev_raster(Poligo, z=10)

Poligo_alt    <- crop(elev, Poligo)                           #   
Poligo_alt   <- Poligo_alt <- mask(Poligo_alt, Poligo)

writeRaster(Anchash_alt, "Raster_manabi.tif")

Geo_data       <-  rasterToPoints(Geo)
Geo_data_frame <-  data.frame(Geo_data)
Geo_data_frame[Geo_data_frame==0] <- NA              # Remplazamos los valores 0 a Na
colnames(Geo_data_frame) = c("lon", "lat", "Geologia")
Geo_data_frame_f <- na.omit(Geo_data_frame)          #  Omotimos los valores Cero

# Isope           <- raster("Data_Raster/ISOPERIODOS.tif")
# Isope_data       <-  rasterToPoints(Isope)
# Isope_data_frame <-  data.frame(Isope_data)
# colnames(Isope_data_frame) = c("lon", "lat", "ISOPERIODOS")
# cortes <- c(1,2,3,4,5)
#------------------------------------------------------------------------

A<- ggplot()+
  geom_raster(data = Geo_data_frame_f,aes(lon,lat, fill = Geologia))+
  scale_fill_distiller(palette   = "Set1", 
                       breaks = c(10,20,30,40,50,60,70,80,90),
                       na.value = 'white',
                       labels = c("[a] ","[b]", "[c]", 
                                  "[d]", "[e]", "f]","[g]","[h]","[i]"),
                       name='Unidad de \nMedida')+
  theme_void() + 
  #annotate(geom = "text", x = -140000, y = ,9360000, label = "oceano", fontface = "italic", color = "grey22", size = 4)+
  annotation_scale(location = "bl", width_hint = 0.4) +
  theme_bw()+
  coord_equal()+
  scale_x_continuous(name=expression(paste("Longitude (",degree,")"))) +
  scale_y_continuous(name=expression(paste("Latitude (",degree,")"))) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  guides(fill = guide_legend(title.position = "top",direction = "vertical",
                             title.theme = element_text(size = 10, colour = "black"),
                             title.hjust = 0.5, raster = TRUE))+
  theme(legend.position = c(0.95,0.80))

slope = terrain(Poligo_alt , opt = "slope") 
aspect = terrain(Poligo_alt , opt = "aspect")
hill = hillShade(slope, aspect, angle = 40, direction = 270)

hill.pa        <-  rasterToPoints(hill)
hill.pa_a      <-  data.frame(hill.pa)
dem.p          <-  rasterToPoints(Poligo_alt)
df             <-  data.frame(dem.p)

colss <-c("#001f54", "#1282a2", "#006400", "#007200", "#008000", "#55a630", "#80b918", "#aacc00", "#bfd200", "#d4d700", "#dddf00", "#eeef20", "#ffff3f")

B= ggplot()+
  geom_raster(data = hill.pa_a, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill() +
  geom_raster(data = Geo_data_frame_f,aes(lon,lat, fill = Geologia), alpha=0.4)+
  scale_fill_distiller(palette   = "Set1", 
                       breaks = c(10,20,30,40,50,60,70,80,90),
                       na.value = 'white',
                       labels = c("[a] ","[b]", "[c]", 
                                  "[d]", "[e]", "f]","[g]","[h]","[i]"),
                       name='Unidad de \nMedida')+
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  labs(x = NULL, y = NULL)+
  theme_bw()+
  theme(axis.text.x  = element_text(face="bold", color="black", size=8),
        axis.text.y  = element_text(angle = 90,face="bold", color="black", size=8),
        panel.border = element_rect(size = 2),
        legend.text =element_text(size=11, family="serif"),
        legend.title = element_text(size=11, family="serif"),
        legend.key.size = unit(0.4, "cm"), 
        legend.key.width = unit(0.9,"cm"),
        legend.position = c(0.95,0.80))

#------------------------------------------------------------------------
ggsave(plot =B ,"Mapa/Mapa de Geologia1.png", units = "cm", 
       width = 29,height = 21, dpi = 1200)






