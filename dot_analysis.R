library(ggplot2)

####################################dark dots###################################
#macro_vein_CB
mvCB_ddots_raw <- read.csv("D:/0. Arbeit/Master/Praktikum Dey Lab/Analysis/Dots/macro_vein_CB/macro_vein_CB_1_dark dots_volume.csv", sep = ";")
View(mvCB_ddots)

mvCB_ddots_raw$mid <- (mvCB_ddots_raw$Bin.Start..mm.. + mvCB_ddots_raw$Bin.End..mm..)/2
mvCB_ddots_raw$width <- (mvCB_ddots_raw$Bin.End..mm.. - mvCB_ddots_raw$Bin.Start..mm..)

mvCB_ddots <- data.frame(µmid = mvCB_ddots_raw$mid * 10^9,
                         µwidth = mvCB_ddots_raw$width * 10^9,
                         value = mvCB_ddots_raw$Value)

ggplot(mvCB_ddots, aes(x = µmid, y = log2(value +1), fill = µmid))+
  geom_col(width = mvCB_ddots$µwidth[1], color = "black")+
  scale_fill_gradientn(colours = c("#440255", "#3D538E", "#228D8B", "#5DC660", "#FCE724"))+
  theme_classic()+
  labs(x = expression(log[2]~Volume~(mu*m^3)), 
       y= "Frequency",
       fill = expression(log[2]~Volume~(mu*m^3)))


#macro_vein_LU
mvLU_ddots_raw <- read.csv("D:/0. Arbeit/Master/Praktikum Dey Lab/Analysis/Dots/macro_vein_LU/macro_vein_LU_1_dark dots_volume.csv", sep = ";")

mvLU_ddots_raw$mid <- (mvLU_ddots_raw$Bin.Start..mm.. + mvLU_ddots_raw$Bin.End..mm..)/2
mvLU_ddots_raw$width <- (mvLU_ddots_raw$Bin.End..mm.. - mvLU_ddots_raw$Bin.Start..mm..)

mvLU_ddots <- data.frame(µmid = mvLU_ddots_raw$mid * 10^9,
                         µwidth = mvLU_ddots_raw$width * 10^9,
                         value = mvLU_ddots_raw$Value)

ggplot(mvLU_ddots, aes(x = µmid, y = log2(value +1), fill = µmid))+
  geom_col(width = mvLU_ddots$µwidth[1], color = "black")+
  scale_fill_gradientn(colours = c("#440255", "#3D538E", "#228D8B", "#5DC660", "#FCE724"))+
  theme_classic()+
  labs(x = expression(log[2]~Volume~(mu*m^3)),
       y = "Frequency",
       fill = expression(log[2]~Volume~(mu*m^3)))


#macro_ff_CB
mffCB_ddots_raw <- read.csv("D:/0. Arbeit/Master/Praktikum Dey Lab/Analysis/Dots/macro_ff_CB/macro_ff_CB_3_dark dots_volume.csv", sep = ";")

mffCB_ddots_raw$mid <- (mffCB_ddots_raw$Bin.Start..mm.. + mffCB_ddots_raw$Bin.End..mm..)/2
mffCB_ddots_raw$width <- (mffCB_ddots_raw$Bin.End..mm.. - mffCB_ddots_raw$Bin.Start..mm..)

mffCB_ddots <- data.frame(µmid = mffCB_ddots_raw$mid * 10^9,
                         µwidth = mffCB_ddots_raw$width * 10^9,
                         value = mffCB_ddots_raw$Value)

ggplot(mffCB_ddots, aes(x = µmid, y = log2(value +1), fill = µmid))+
  geom_col(width = mffCB_ddots$µwidth[1], color = "black")+
  scale_fill_gradientn(colours = c("#440255", "#3D538E", "#228D8B", "#5DC660", "#FCE724"))+
  theme_classic()+
  labs(x = expression(log[2]~Volume~(mu*m^3)),
       y = "Frequency",
       fill = expression(log[2]~Volume~(mu*m^3)))


#macro_ff_LU
mffLU_ddots_raw <- read.csv("D:/0. Arbeit/Master/Praktikum Dey Lab/Analysis/Dots/macro_ff_LU/macro_ff_LU_2_dark dots_volume.csv", sep = ";")

mffLU_ddots_raw$mid <- (mffLU_ddots_raw$Bin.Start..mm.. + mffLU_ddots_raw$Bin.End..mm..)/2
mffLU_ddots_raw$width <- (mffLU_ddots_raw$Bin.End..mm.. - mffLU_ddots_raw$Bin.Start..mm..)

mffLU_ddots <- data.frame(µmid = mffLU_ddots_raw$mid * 10^9,
                          µwidth = mffLU_ddots_raw$width * 10^9,
                          value = mffLU_ddots_raw$Value)

ggplot(mffLU_ddots, aes(x = µmid, y = log2(value +1), fill = µmid))+
  geom_col(width = mffLU_ddots$µwidth[1], color = "black")+
  scale_fill_gradientn(colours = c("#440255", "#3D538E", "#228D8B", "#5DC660", "#FCE724"))+
  theme_classic()+
  labs(x = expression(log[2]~Volume~(mu*m^3)),
       y = "Frequency",
       fill = expression(log[2]~Volume~(mu*m^3)))


#macro_light_CB
mlCB_ddots_raw <- read.csv("D:/0. Arbeit/Master/Praktikum Dey Lab/Analysis/Dots/macro_light_CB/macro_light_CB_1_dots_volume.csv", sep = ";")

mlCB_ddots_raw$mid <- (mlCB_ddots_raw$Bin.Start..mm.. + mlCB_ddots_raw$Bin.End..mm..)/2
mlCB_ddots_raw$width <- (mlCB_ddots_raw$Bin.End..mm.. - mlCB_ddots_raw$Bin.Start..mm..)

mlCB_ddots <- data.frame(µmid = mlCB_ddots_raw$mid * 10^9,
                          µwidth = mlCB_ddots_raw$width * 10^9,
                          value = mlCB_ddots_raw$Value)

ggplot(mlCB_ddots, aes(x = µmid, y = log2(value +1), fill = µmid))+
  geom_col(width = mlCB_ddots$µwidth[1], color = "black")+
  scale_fill_gradientn(colours = c("#440255", "#3D538E", "#228D8B", "#5DC660", "#FCE724"))+
  theme_classic()+
  labs(x = expression(log[2]~Volume~(mu*m^3)),
       y = "Frequency",
       fill = expression(log[2]~Volume~(mu*m^3)))



#################################sphericity#####################################
#macro_vein_CB
mvCB_sphddots_raw <- read.csv("D:/0. Arbeit/Master/Praktikum Dey Lab/Analysis/Dots/macro_vein_CB/macro_vein_CB_1_dark dots_sphericity.csv", sep = ";")
View(mvCB_sphddots_raw)
mvCB_sphddots_raw$mid <- (mvCB_sphddots_raw$Bin.Start + mvCB_sphddots_raw$Bin.End)/2
mvCB_sphddots_raw$width <- (mvCB_sphddots_raw$Bin.End - mvCB_sphddots_raw$Bin.Start)

mvCB_sphddots <- data.frame(mid = mvCB_sphddots_raw$mid,
                         width = mvCB_sphddots_raw$width,
                         value = mvCB_sphddots_raw$Value)

ggplot(mvCB_sphddots, aes(x = mid, y = log2(value +1), fill = mid))+
  geom_col(width = mvCB_sphddots$width[1], color = "black")+
  scale_fill_gradientn(colours = c("#440255", "#3D538E", "#228D8B", "#5DC660", "#FCE724"))+
  theme_classic()+
  labs(x = "Sphericity",
       y = "Frequency",
       fill = "sphericity")


#macro_vein_LU
mvLU_sphddots_raw <- read.csv("D:/0. Arbeit/Master/Praktikum Dey Lab/Analysis/Dots/macro_vein_LU/macro_vein_LU_1_dark dots_sphericity.csv", sep = ";")

mvLU_sphddots_raw$mid <- (mvLU_sphddots_raw$Bin.Start + mvLU_sphddots_raw$Bin.End)/2
mvLU_sphddots_raw$width <- (mvLU_sphddots_raw$Bin.End - mvLU_sphddots_raw$Bin.Start)

mvLU_sphddots <- data.frame(mid = mvLU_sphddots_raw$mid,
                            width = mvLU_sphddots_raw$width,
                            value = mvLU_sphddots_raw$Value)

ggplot(mvLU_sphddots, aes(x = mid, y = log2(value +1), fill = mid))+
  geom_col(width = mvLU_sphddots$width[1], color = "black")+
  scale_fill_gradientn(colours = c("#440255", "#3D538E", "#228D8B", "#5DC660", "#FCE724"))+
  theme_classic()+
  labs(x = "Sphericity",
       y = "Frequency",
       fill = "sphericity")


#macro_ff_CB
mffCB_sphddots_raw <- read.csv("D:/0. Arbeit/Master/Praktikum Dey Lab/Analysis/Dots/macro_ff_CB/macro_ff_CB_3_dark dots_sphericity.csv", sep = ";")

mffCB_sphddots_raw$mid <- (mffCB_sphddots_raw$Bin.Start + mffCB_sphddots_raw$Bin.End)/2
mffCB_sphddots_raw$width <- (mffCB_sphddots_raw$Bin.End - mffCB_sphddots_raw$Bin.Start)

mffCB_sphddots <- data.frame(mid = mffCB_sphddots_raw$mid,
                            width = mffCB_sphddots_raw$width,
                            value = mffCB_sphddots_raw$Value)

ggplot(mffCB_sphddots, aes(x = mid, y = log2(value +1), fill = mid))+
  geom_col(width = mffCB_sphddots$width[1], color = "black")+
  scale_fill_gradientn(colours = c("#440255", "#3D538E", "#228D8B", "#5DC660", "#FCE724"))+
  theme_classic()+
  labs(x = "Sphericity",
       y = "Frequency",
       fill = "sphericity")


#macro_ff_LU
mffLU_sphddots_raw <- read.csv("D:/0. Arbeit/Master/Praktikum Dey Lab/Analysis/Dots/macro_ff_LU/macro_ff_LU_2_dark dots_sphericity.csv", sep = ";")

mffLU_sphddots_raw$mid <- (mffLU_sphddots_raw$Bin.Start + mffLU_sphddots_raw$Bin.End)/2
mffLU_sphddots_raw$width <- (mffLU_sphddots_raw$Bin.End - mffLU_sphddots_raw$Bin.Start)

mffLU_sphddots <- data.frame(mid = mffLU_sphddots_raw$mid,
                            width = mffLU_sphddots_raw$width,
                            value = mffLU_sphddots_raw$Value)

ggplot(mffLU_sphddots, aes(x = mid, y = log2(value +1), fill = mid))+
  geom_col(width = mffLU_sphddots$width[1], color = "black")+
  scale_fill_gradientn(colours = c("#440255", "#3D538E", "#228D8B", "#5DC660", "#FCE724"))+
  theme_classic()+
  labs(x = "Sphericity",
       y = "Frequency",
       fill = "sphericity")


#macro_light_CB
mlCB_sphddots_raw <- read.csv("D:/0. Arbeit/Master/Praktikum Dey Lab/Analysis/Dots/macro_light_CB/macro_light_CB_1_dots_sphericity.csv", sep = ";")

mlCB_sphddots_raw$mid <- (mlCB_sphddots_raw$Bin.Start + mlCB_sphddots_raw$Bin.End)/2
mlCB_sphddots_raw$width <- (mlCB_sphddots_raw$Bin.End - mlCB_sphddots_raw$Bin.Start)

mlCB_sphddots <- data.frame(mid = mlCB_sphddots_raw$mid,
                             width = mlCB_sphddots_raw$width,
                             value = mlCB_sphddots_raw$Value)

ggplot(mlCB_sphddots, aes(x = mid, y = log2(value +1), fill = mid))+
  geom_col(width = mlCB_sphddots$width[1], color = "black")+
  scale_fill_gradientn(colours = c("#440255", "#3D538E", "#228D8B", "#5DC660", "#FCE724"))+
  theme_classic()+
  labs(x = "Sphericity",
       y = "Frequency",
       fill = "sphericity")




#####################################bright dots################################

#macro_vein_CB
mvCB_bdots_raw <- read.csv("D:/0. Arbeit/Master/Praktikum Dey Lab/Analysis/Dots/macro_vein_CB/macro_vein_CB_1_bright dots_volumes.csv", sep = ";")

mvCB_bdots_raw$mid <- (mvCB_bdots_raw$Bin.Start..mm.. + mvCB_bdots_raw$Bin.End..mm..)/2
mvCB_bdots_raw$width <- (mvCB_bdots_raw$Bin.End..mm.. - mvCB_bdots_raw$Bin.Start..mm..)

mvCB_bdots <- data.frame(µmid = mvCB_bdots_raw$mid * 10^9,
                         µwidth = mvCB_bdots_raw$width * 10^9,
                         value = mvCB_bdots_raw$Value)

ggplot(mvCB_bdots, aes(x = µmid, y = log2(value +1), fill = µmid))+
  geom_col(width = mvCB_bdots$µwidth[1], color = "black")+
  scale_fill_gradientn(colours = c("#440255", "#3D538E", "#228D8B", "#5DC660", "#FCE724"))+
  theme_classic()+
  labs(x = expression(log[2]~Volume~(mu*m^3)), 
       y= "Frequency",
       fill = expression(log[2]~Volume~(mu*m^3)))



#macro_vein_LU
mvLU_bdots_raw <- read.csv("D:/0. Arbeit/Master/Praktikum Dey Lab/Analysis/Dots/macro_vein_LU/macro_vein_LU_1_bright dots_volume.csv", sep = ";")

mvLU_bdots_raw$mid <- (mvLU_bdots_raw$Bin.Start..mm.. + mvLU_bdots_raw$Bin.End..mm..)/2
mvLU_bdots_raw$width <- (mvLU_bdots_raw$Bin.End..mm.. - mvLU_bdots_raw$Bin.Start..mm..)

mvLU_bdots <- data.frame(µmid = mvLU_bdots_raw$mid * 10^9,
                         µwidth = mvLU_bdots_raw$width * 10^9,
                         value = mvLU_bdots_raw$Value)

ggplot(mvLU_bdots, aes(x = µmid, y = log2(value +1), fill = µmid))+
  geom_col(width = mvLU_bdots$µwidth[1], color = "black")+
  scale_fill_gradientn(colours = c("#440255", "#3D538E", "#228D8B", "#5DC660", "#FCE724"))+
  theme_classic()+
  labs(x = expression(log[2]~Volume~(mu*m^3)), 
       y= "Frequency",
       fill = expression(log[2]~Volume~(mu*m^3)))


#macro_ff_CB
mffCB_bdots_raw <- read.csv("D:/0. Arbeit/Master/Praktikum Dey Lab/Analysis/Dots/macro_ff_CB/macro_ff_CB_3_bright dots_volume.csv", sep = ";")

mffCB_bdots_raw$mid <- (mffCB_bdots_raw$Bin.Start..mm.. + mffCB_bdots_raw$Bin.End..mm..)/2
mffCB_bdots_raw$width <- (mffCB_bdots_raw$Bin.End..mm.. - mffCB_bdots_raw$Bin.Start..mm..)

mffCB_bdots <- data.frame(µmid = mffCB_bdots_raw$mid * 10^9,
                          µwidth = mffCB_bdots_raw$width * 10^9,
                          value = mffCB_bdots_raw$Value)

ggplot(mffCB_bdots, aes(x = µmid, y = log2(value +1), fill = µmid))+
  geom_col(width = mffCB_bdots$µwidth[1], color = "black")+
  scale_fill_gradientn(colours = c("#440255", "#3D538E", "#228D8B", "#5DC660", "#FCE724"))+
  theme_classic()+
  labs(x = expression(log[2]~Volume~(mu*m^3)),
       y = "Frequency",
       fill = expression(log[2]~Volume~(mu*m^3)))


#macro_ff_LU
mffLU_bdots_raw <- read.csv("D:/0. Arbeit/Master/Praktikum Dey Lab/Analysis/Dots/macro_ff_LU/macro_ff_LU_2_bright dots_volume.csv", sep = ";")

mffLU_bdots_raw$mid <- (mffLU_bdots_raw$Bin.Start..mm.. + mffLU_bdots_raw$Bin.End..mm..)/2
mffLU_bdots_raw$width <- (mffLU_bdots_raw$Bin.End..mm.. - mffLU_bdots_raw$Bin.Start..mm..)

mffLU_bdots <- data.frame(µmid = mffLU_bdots_raw$mid * 10^9,
                          µwidth = mffLU_bdots_raw$width * 10^9,
                          value = mffLU_bdots_raw$Value)

ggplot(mffLU_bdots, aes(x = µmid, y = log2(value +1), fill = µmid))+
  geom_col(width = mffLU_bdots$µwidth[1], color = "black")+
  scale_fill_gradientn(colours = c("#440255", "#3D538E", "#228D8B", "#5DC660", "#FCE724"))+
  theme_classic()+
  labs(x = expression(log[2]~Volume~(mu*m^3)),
       y = "Frequency",
       fill = expression(log[2]~Volume~(mu*m^3)))


