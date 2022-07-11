library(readxl)
library(sf)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(viridis)
library(RColorBrewer)
library(visdat)


<<<<<<< HEAD

setwd("~/Current Work/R_projects/BLOCKING_GIS_2")
=======
setwd("~/Current Work/R_Projects/BLOCKING_GIS_2")
>>>>>>> bf0a71558cf96c12afd0396e826b98b667b1e973
toMatch <- c("~")
file.list_gis <- list.files(path = './FACE_MAPPING_GIS', pattern = '.xlsx', recursive = TRUE, full.names = TRUE)
file.list_gis <- file.list_gis[!grepl(paste(toMatch,collapse="|"), file.list_gis)]


########### Loading Face maps and coordinates ############


face_map_gis<- function(i) {
  x <- read_xlsx(i,sheet = 1)
  
  colnames(x) <-
    c(
      "c1",
      "c2",
      "c3",
      "c4",
      "c5",
      "c6",
      "c7",
      "c8",
      "c9",
      "c10",
      "c11",
      "c12",
      "c13"
    )
  
  x <- x %>% transmute(
    HOLE_ID = as.character(c1),
    LOCATIONX = as.numeric(c2),
    LOCATIONY = as.numeric(c3),
    LOCATIONZ = as.numeric(c4),
    LENGTH = as.numeric(c5),
    LEVEL = as.numeric(c6),
    AREA = as.character(c7),
    ROCKCODE = as.character(c8),
    SAMP_BY = as.character(c9),
    TENEMENT = as.character(c11),
    file = i[1],
    file_hole = paste(HOLE_ID, file, sep = " ")) %>%
    dplyr::filter(!is.na(HOLE_ID)) %>%
    distinct()
  
  
} 

############# Applying Function to the file list gis ###############
df_gis_coords <- lapply(file.list_gis, face_map_gis) %>%
  bind_rows %>%
  as.data.frame() %>%
  distinct(.keep_all = TRUE)

########### Adding Vein#########
for  (j in c("WGS","DCX","STF","DNA HWS","DMI","DNC","MIHW","DNF","MI","DNJ","DNM","MST_FWS","MST","SDN", "SDN3", "SDN2","SDN2 SPLIT","SDN2S","SDN4", "SDN4 SPLIT","MST2", "MAS FWS", "MAS","MHWS" ,"MAI","MAIHWS", "MAI_HWS","MAI HWS", "MAIS", "BNZ", "BHWS" , "JES" , "SDY", "BBK", "BIBAK","SDN SPLIT","SDY SPLIT", "SDNS","MST_SPLIT","MST2_FWS","WGS","STV","STF","SDN4S")) {
  df_gis_coords[grepl(j,df_gis_coords$HOLE_ID),"VEIN"] <- j
}

VEIN_NAME <- c(c( "SDN4S","MAIS","DNJ","DNF","SDN", "SDN3", "SDN2","SDN2 SPLIT","SDN2S","SDN4", "SDN4 SPLIT","MST2", "MAS FWS", "MAS","MHWS" ,"MAI","MAIHWS", "MAI_HWS","MAI HWS", "MAIS", "BNZ", "BHWS" , "JES" , "SDY", "BBK", "BIBAK","SDN SPLIT","SDY SPLIT", "SDNS","MST_SPLIT","MST2_FWS"))
ROCK_CODE <- c(c( "181","222","520","510","140", "170", "150","151","151","180", "000","420", "421", "120","121" ,"220","221", "221","221", "000", "110", "113" , "160" , "140", "130", "130","140 - 000","140 - 000", "140 - 000","420 - 000","421"))

VEIN_ROCK_CODE <- cbind(VEIN_NAME, ROCK_CODE) %>% as.data.frame()



# ifelse(VEIN == "SDY","SDN",VEIN)


df_gis_coords <- df_gis_coords %>% mutate(VEIN = ifelse(VEIN == "SDY","SDN",
                                                        ifelse(VEIN == "MAI_HWS","MAIHWS",
                                                               ifelse(VEIN == "MAI HWS","SDN",
                                                                      ifelse(VEIN == "MST_SPLIT","MST2FWS",
                                                                             ifelse(VEIN == "MST2_FWS","MST2FWS",
                                                                                    ifelse(VEIN == "DCX","DNC",VEIN)))))))

df_gis_coords <- left_join(df_gis_coords,VEIN_ROCK_CODE, by = c("VEIN" = "VEIN_NAME"))

df_gis_coords <- df_gis_coords %>% 
  mutate(fn_ROCKCODE =ifelse(is.na(ROCKCODE), paste(VEIN,ROCK_CODE, sep = " "),paste(VEIN,ROCKCODE, sep = " ")))


########### Loading Assay Values ###############
face_map_gis_assay<- function(i) {
  x = read_xlsx(i,sheet = 2)
  
  
  colnames(x) <-
    c(
      "c1",
      "c2",
      "c3",
      "c4",
      "c5",
      "c6",
      "c7",
      "c8",
      "c9",
      "c10",
      "c11",
      "c12",
      "c13",
      "c14",
      "c15",
      "c16",
      "c17",
      "c18",
      "c19",
      "c20",
      "c21",
      "c22",
      "c23",
      "c24",
      "c25"
    )
  
  x <- x %>% transmute(
    HOLE_ID = as.character(c1),
    FROM = as.numeric(c2),
    TO = as.numeric(c3),
    LENGTH = as.numeric(c4),
    SAMPLE_NO = as.numeric(c5),
    AU_GPT = as.numeric(c6),
    AG_GPT = as.numeric(c12),
    CU_PPM = as.numeric(c7),
    PB_PPM = as.numeric(c8),
    ZN_PPM = as.numeric(c9),
    LEN_AU = LENGTH * AU_GPT,
    LEN_AG = LENGTH * AG_GPT,
    LEN_CU = LENGTH * CU_PPM,
    LEN_PB = LENGTH * PB_PPM,
    LEN_ZN = LENGTH * ZN_PPM,
    SG = as.numeric(c10),
    REASSAY = as.numeric(c11),
    ROCK_TYPE = as.character(c13),
    MV_WIDTH = as.numeric(c14),
    BATCH_NO = as.character(c17),
    file = i[1],
    duplicate = paste(HOLE_ID,AU_GPT,AG_GPT,CU_PPM,PB_PPM,ZN_PPM,file)) %>%
    dplyr::filter(!is.na(HOLE_ID)) %>%
    distinct()
  
} 




############# Applying Function to the file list gis ###############
df_gis_assay_raw <- lapply(file.list_gis, face_map_gis_assay) %>%
  bind_rows %>%
  as.data.frame() %>%
  distinct(.keep_all = TRUE)
df_gis_assay_filter <- df_gis_assay_raw %>% filter(file == "./FACE_MAPPING_GIS/OLD/UGFS_2020.xlsx")

##########################

df_gis_assay <- df_gis_assay_raw %>% group_by(HOLE_ID,ROCK_TYPE) %>%
  summarize(LENGTH = sum(LENGTH),
            AU = sum(LEN_AU)/ sum(LENGTH),
            AU = ifelse(AU >=25 ,25,AU),
            AG = sum(LEN_AG)/ sum(LENGTH),
            CU = sum(LEN_CU)/ sum(LENGTH),
            ZN = sum(LEN_ZN)/ sum(LENGTH),
            PB = sum(LEN_PB)/ sum(LENGTH),
            file = file) %>%
  mutate(LEN_AU = LENGTH * AU)


MV_L <- df_gis_assay %>%
  group_by(HOLE_ID) %>% 
  summarize(MV_L = ifelse(ROCK_TYPE == "MV",LENGTH,0 )) %>%
  dplyr::filter(MV_L != 0)


df_gis_assay <- left_join(df_gis_assay, MV_L, by = "HOLE_ID")

df_gis_assay <- df_gis_assay %>% mutate(LENGTH = ifelse(ROCK_TYPE != "MV", (3 - MV_L)/2, MV_L),
                                        LEN_AU = LENGTH * AU)


df_gis_assay_block <- df_gis_assay %>% filter (!is.na(AU)) %>%
  group_by(HOLE_ID) %>% 
  summarize(COMP_AU = sum(LEN_AU)/sum(LENGTH))


df_gis_assay_block$cat <- cut(df_gis_assay_block$COMP_AU,
                              breaks=c(0,1, 3, 5, 10, 15,25),
                              labels=c('0 - 1', '1 - 3', '3 - 5', '5 - 10', '10 - 15', '15 - 25'))


############# Join Coordinates and assay##############

df_joined <- left_join(df_gis_assay_block ,df_gis_coords,by = "HOLE_ID")
df_joined <- df_joined %>% dplyr::filter(!is.na(LOCATIONX))


############ plotting of df files ############


df_points <- st_as_sf(df_joined , coords = c("LOCATIONX", "LOCATIONY"), crs = 3125)

df_joined_XY <- df_joined %>% select(HOLE_ID,LOCATIONX,LOCATIONY)

df_points <-left_join (df_points,df_joined_XY, by = "HOLE_ID") 

face_map_plot <- ggplot(data = df_points, aes(color = COMP_AU, text = HOLE_ID)) + 
  geom_sf(size = 2.0) +
  scale_colour_gradientn(colours = c("purple","red","#FF5349","yellow", "green", "blue", "gray"),
                         values = c(1.0,0.4,0.32,0.2,0.12,0.04,0))


############ plotly



col <- colorRamp(c("gray","blue","green","yellow","red","purple"))


# plot <- plot_ly(data = df_points) %>% 
#   add_sf(type = "scatter", color = ~fn_ROCKCODE, text = ~HOLE_ID)%>% 
#   layout(title = "AMCI Face Samples",
#          plot_bgcolor='#e5ecf6', 
#          xaxis = list( 
#            zerolinecolor = '#ffff', 
#            zerolinewidth = 2, 
#            gridcolor = 'ffff'), 
#          yaxis = list( 
#            zerolinecolor = '#ffff', 
#            zerolinewidth = 2, 
#            gridcolor = 'ffff') )
# 
# plot


############ INPUT SHAPEFILE POSITION LINES - N_S_ ###############  

<<<<<<< HEAD
setwd("~/Current Work/R_projects/BLOCKING_GIS_2/Shapefiles")
=======
setwd("~/Current Work/R_Projects/BLOCKING_GIS_2/Shapefiles")
>>>>>>> bf0a71558cf96c12afd0396e826b98b667b1e973


POS_LINES_N_S <- st_read(
  "./N_S_Pos.shp")
POS_LINES_N_S<- POS_LINES_N_S [,-c(1)]
POS_LINES_PLOT <- ggplot() + 
  geom_sf(data = POS_LINES_N_S , size = 0.1, color = "cyan") + 
  ggtitle("POS_LINES_PLOT") + 
  coord_sf()


colnames(POS_LINES_N_S) <-
  c("POS_N_S","geometry")

plot(POS_LINES_N_S)


############ INPUT SHAPEFILE POSITION LINES - E_W ###############  

setwd("~/Current Work/R_projects/BLOCKING_GIS_2/Shapefiles")


POS_LINES_E_W<- st_read(
  "./E_W_Pos.shp")
POS_LINES_E_W<- POS_LINES_E_W[,-c(1)]
POS_LINES_PLOT <- ggplot() + 
  geom_sf(data = POS_LINES_E_W, size = 0.1, color = "cyan") + 
  ggtitle("POS_LINES_PLOT") + 
  coord_sf()


colnames(POS_LINES_E_W) <-
  c("POS_N_S","geometry")

plot(POS_LINES_E_W)


############# Intersection of the position lines and the face mapping plots #############
POS_FACE_MAP <- st_intersection(POS_LINES_N_S,df_points)

#### Remove duplicates #####
POS_FACE_MAP_2 <- POS_FACE_MAP %>% 
  dplyr::distinct(HOLE_ID,COMP_AU,LEVEL,.keep_all = TRUE)

###### For positioning along N-S position #######
POS_FACE_MAP <- POS_FACE_MAP_2 %>% 
  mutate(BLOCK_LOCATIONX =  (LOCATIONX-((LOCATIONY - 815635.8096)/(tan(40*pi/180)))-614923.6274)/155.5724*10)


############## Compositing per block ##############

POS_FACE_MAP_AVERAGE <- POS_FACE_MAP %>% 
  dplyr::filter(!is.na(COMP_AU)) %>%
  group_by(POS_N_S, fn_ROCKCODE, LEVEL) %>% 
  summarize(AVE= mean(COMP_AU)) %>% mutate(AVE = signif(AVE,3))


setwd("~/Current Work/R_projects/BLOCKING_GIS_2")

