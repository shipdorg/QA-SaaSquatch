

## Julia Haywood - Biodiversify - 02-08-2022

## Edited by: anna@orbica.world - 25-10-2022

## Biodiversity tool - summary plots

##############################################################################

# All data required located in - C:\Users\julia\Biodiversify\Biodiversify - Documents\Biodiversify\Projects\Automation - Strategic site screening\Data\Data for tool


###############################################################################

# Prepare script
library(sf)
library(dplyr)
library(tidyr)
library(jsonlite)
library(showtext)
##################
# Set to TRUE if you are debuggging locally, 
# For production version -> set to FALSE! (DO NOT PUSH "TRUE" to the repo!)
# Attention: It also deactivates writing to the database
isLocalDev = TRUE
###################

if(isLocalDev == TRUE){
  ####### LOCAL DEVELOPMENT ########
  print("######### DEBUGGING ##########")
  # Set working directory
  library(rstudioapi)
  # setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  wd_path <- getwd()
  config_dir <- getwd()
  # Arguments - local version
  batch_id <- "b8b6deee-57e1-4855-8ef0-53893f64a5a5"
  output_path <- "./out"
  ###### LOCAL DEVELOPMENT END #######
} else {
  ###### FME DEVELOPMENT ########
  # Get arguments from command line
  # 1th: working dir
  # 2nd: batch id
  # 3rd: output path of plots
  args <- commandArgs(trailingOnly=TRUE)
  if (length(args)==0) {
    stop("At least one argument must be supplied", call.=FALSE)
  }
  config_dir <- args[1]
  batch_id <- args[2]
  output_path <- args[3]
  setwd(output_path)
  ###### FME DEVELOPMENT ########
}
source(sprintf("%s/postgres_connection.R", config_dir))
fontPath <- sprintf("%s/fme/fonts/Lato-Regular.ttf", config_dir)
#font_add(family = "Lato", regular = fontPath)
font_add_google("Lato") #if local path does not work, use this

# Paths
plot1_path <- sprintf("%s/plot1.png", output_path)
plot2_path <- sprintf("%s/plot2.png", output_path)
plot3_path <- sprintf("%s/plot3.png", output_path)
plot4_path <- sprintf("%s/plot4.png", output_path)
plot5_path <- sprintf("%s/plot5.png", output_path)
plot6_path <- sprintf("%s/plot6.png", output_path)

# Helper functions
jsonToDf <- function (JSONcol){
  df1 <- data.frame()
	# creates dataframe from JSOn records
	for (row in 1:length(JSONcol)){
		# if valid JSON (not empty)
		if (validate(JSONcol[row])) {
			if (exists("df1")){
				df1 <- rbind(df1, fromJSON(JSONcol[row]))
			} else if (exists("JSONcol[row]")) {
				df1 <- fromJSON(JSONcol[row])
			}			
		} 
	}
  return(df1)
}

# Processing
dbConnection <- connectDB(config_dir)
if (exists("dbConnection") && !inherits(dbConnection, "try-error")) {
  ## Site
  
  qOptions <- sprintf("SELECT id, aoi_label, batch_id FROM public.processing_queue
  WHERE batch_id = '%s' ORDER BY upload_time DESC", batch_id)
  Options <- dbGetQuery(dbConnection, qOptions)
  
	# Sort by id
	Options <- Options[order(Options$aoi_label),]         # Order by Site label
	# Add Habitat group
	qBaseline_Habs <- sprintf("SELECT r.baseline_habitats_area FROM public.processing_results r JOIN public.processing_queue q ON r.site_id = q.id 
      WHERE r.batch_id = '%s' ORDER BY q.aoi_label ASC;", batch_id)
	Baseline_HabsJSON <- dbGetQuery(dbConnection, qBaseline_Habs)
	Baseline_Habs <- jsonToDf(Baseline_HabsJSON$baseline_habitats_area)
	
	if(dim(Options)[1] > 1) {
	  
  	##############################################################################
	  
  	####################################### Biodiversity habitats - Area	
	  
  	##### Prepare data
  
  	# Add missing options
  	missing <- setdiff(Options$aoi_label, Baseline_Habs$`Site Label`)
  	Baseline_Habs[nrow(Baseline_Habs) + seq_along(missing), "Site Label"] <- missing
  	Baseline_Habs[is.na(Baseline_Habs)] <- 0
  	
  	qBM_Habs <- "SELECT * FROM refdata.biodiv_habitats_groups"
  	BM_Habs <- dbGetQuery(dbConnection, qBM_Habs)
  
  	Baseline_Habs <- base::merge(Baseline_Habs, BM_Habs, by.x = c('Habitat'), by.y = c('habitat'), all.x = TRUE, all.y = FALSE)              # Assign habitat group
  	
  	Baseline_Habs$`Maximum area of habitat in site (ha)`[Baseline_Habs$`Maximum area of habitat in site (ha)` == '-'] <- 0
  	Baseline_Habs <- subset(Baseline_Habs,
  					 select=c(Id,`Site Label`, habitat_group,`Maximum area of habitat in site (ha)`))  # Simplify table
  	Baseline_Habs$`Maximum area of habitat in site (ha)` <- as.numeric(Baseline_Habs$`Maximum area of habitat in site (ha)`) 
  	Baseline_Habs <- Baseline_Habs %>% group_by(`Site Label`, habitat_group) %>%
  					 summarise(`Maximum area of habitat in site (ha)` =
  					 sum(`Maximum area of habitat in site (ha)`))                     # Group amount by Site and habitat group
  	Baseline_Habs <- Baseline_Habs[order(Baseline_Habs$`Site Label`),]                                          # Order Site_ID by numeric
  
  	# Create colour pallette
  	
  	habitat_group <- c("Coastal","Cropland","Grassland","Heathland and shrub",
  	                   "Intertidal sediment","Lakes","Rocky shore ",
  	                   "Sparsely vegetated land","Urban habitat","Wetland",
  	                   "Woodland and forest")
  	Colour <- c("#FBC740","#595221","#A9BF6C","#B9A7BB","#E6E0D9","#6DB8CA",
  	            "#8A9EA0","#B85450","#d1909a","#4F946F","#1C442C")
  	Hab_colours <- data.frame(habitat_group, Colour, stringsAsFactors=FALSE)
  	
  	######
  	# Make df wide
  	
  	Baseline_Habs_wide <- spread(Baseline_Habs,`Site Label`,
  	                             `Maximum area of habitat in site (ha)`)                          # df long to wide
  	# Remove habitats with 0 label -> prevents the barchart showing wrong colors
  	if (dim(Baseline_Habs_wide)[1] > 1) {
  	  Baseline_Habs_wide <- Baseline_Habs_wide[Baseline_Habs_wide$habitat_group != 0,]
  	}
  	Hab_colours <- Hab_colours[(Hab_colours$habitat_group %in%
  	                              Baseline_Habs_wide$habitat_group),]                            # Remove any colours for missing habitat groups
  	Baseline_Habs_wide <- subset(Baseline_Habs_wide, select=-c(habitat_group))      # Simplify table
  	Baseline_Habs_wide[is.na(Baseline_Habs_wide)] <- 0                              # Replace NAs with 0 
  
  	##### Create plot
  	print("Creating plot1...")
  	png(filename=plot1_path, width = 750, height = 480,
  	    units = "px", family = "Lato")
  	showtext_begin() # Apply custom font
  	par(mar = c(8,6.5,3,20), xpd=T, bg="#FFFFFF")                                                # Set plot extent
  	barplot(as.matrix(Baseline_Habs_wide),xaxt="n",yaxt="n",col.axis="#2b444b")                        # Create temporary extent
  	rect(par("usr")[1], par("usr")[3],
  	     par("usr")[2], par("usr")[4],
  	     col = "#E4ECED")                                                           # Color background
  	par(new = TRUE)                                                                 # Add new plot
  	if (max(Baseline_Habs$`Maximum area of habitat in site (ha)`)== 0){
  	  barplot(as.matrix(Baseline_Habs_wide),col=Hab_colours$Colour, las=2,cex.axis=1.2,
  	        ylab = "", xlab = '',border=NA,col.axis = "#2C444B", ylim=c(0,5), col.ticks = "#2C444B",
  	        col.lab = "#2C444B", main= "Area of habitat groups in each site", col.main="#2C444B")    # Create plot
  	} else {
  	  barplot(as.matrix(Baseline_Habs_wide),col=Hab_colours$Colour, las=2,cex.axis=1.2,
  	        ylab = "", xlab = '',border=NA,col.axis = "#2C444B", col.ticks = "#2C444B",
  	        col.lab = "#2C444B", main= "Area of habitat groups in each site", col.main="#2C444B")    # Create plot
  	}
  	title(ylab = "Area (ha)", cex.lab = 1.2,line = 4,col.lab = '#2C444B')             # ylabel
  	box(col="#2C444B")                                                              # Assign box and colour                                  # Add legend                                  # Add legend                                # Add legend
  	if(dim(Hab_colours)[1] != 0){
  	  legend("right", 
  	         legend=Hab_colours$habitat_group, 
  	         col=Hab_colours$Colour, 
  	         pch = c(15), 
  	         bty = "n", 
  	         pt.cex = 2, 
  	         cex = 1.2, 
  	         text.col = "#2C444B", 
  	         inset = c(-0.7, 0),
  	         ncol=1,
  	         xpd=T)                                 # Add legend
  	}
    # Saved as plot 1
  	showtext_end()
  	dev.off()
  	        
  	##############################################################################
  	
  	####################################### Biodiversity units - Area
  	
  	##### Prepare data
  	qArea_BU <- sprintf("SELECT r.biodiversity_units_area, q.aoi_label FROM public.processing_results r JOIN public.processing_queue q ON r.site_id = q.id 
      WHERE r.batch_id = '%s' ORDER BY q.aoi_label ASC;", batch_id)
  	Area_BU_JSON <- dbGetQuery(dbConnection, qArea_BU)
  	
  	Area_BU <- jsonToDf(Area_BU_JSON$biodiversity_units_area)
  	
  	Area_BU$row_num <- seq.int(nrow(Area_BU))                                       # Assign numeric id
  	Sites <- as.factor(Area_BU$`Site Label`)                                             # Set as factor
  	
  	if (max(Area_BU$`Area Biodiversity Units`)== 0){
  	  maxY <- 5
  	} else {
  	  maxY <- max(Area_BU$`Area Biodiversity Units`)
  	}
  	
  	##### Create plot
  	print("Creating plot2...")
  	png(filename=plot2_path,  width = 480, height = 480,
  	    units = "px", family="Lato")
  	showtext_begin()
  	par(mar = c(8,6.5,3,1), bg="#FFFFFF")                                                           # Set plot extent
  	plot(Area_BU$row_num, Area_BU$`Area Biodiversity Units`,
  	     xaxt="n",yaxt="n",xlab="",ylab="",col.axis="#2C444B")                                         # Create temporary extent
  	rect(par("usr")[1], par("usr")[3],
  	     par("usr")[2], par("usr")[4],
  	     col = "#E4ECED")                                                           # Color background
  	par(new = TRUE)                                                                 # Add new plot
  	plot(Area_BU$row_num, Area_BU$`Area Biodiversity Units`, las=1,cex.axis=1.2,
  	     ylab = "", pch=16, col='#2C444B',col.ticks = "#2C444B",
  	     xlab = '',xaxt = 'n',col.axis="#2C444B", ylim=c(0,maxY),
  	     main="Biodiversity units - Area habitats", cex=1.5, col.main="#2C444B")                                            # Create plot
  	axis(side=1, at=Area_BU$row_num, cex.axis=1.2,labels = Sites,las=2,col.axis="#2C444B", col.ticks = "#2C444B")       # Add axis
  	title(ylab = "Biodiversity Units", cex.lab = 1.2,line = 4,
  	      col.lab = '#2C444B')                                                      # ylabel
  	# Saved as plot 2
  	showtext_end()
  	dev.off()
  	
  	####################################### Biodiversity units - Linear
  	
  	##### Prepare data
  	qLinear_BU <- sprintf("SELECT r.biodiversity_units_linear, q.aoi_label FROM public.processing_results r JOIN public.processing_queue q ON r.site_id = q.id 
      WHERE r.batch_id = '%s' ORDER BY q.aoi_label ASC;", batch_id)
  	Linear_BU_JSON <- dbGetQuery(dbConnection, qLinear_BU)
  	Linear_BU <- jsonToDf(Linear_BU_JSON$biodiversity_units_linear)
  	
  	# Linear_BU <- Linear_BU[order(Linear_BU$aoi_label),]  # Order by lLabel
  	Linear_BU$row_num <- seq.int(nrow(Linear_BU))                                   # Assign numeric id
  	Sites <- as.factor(Linear_BU$`Site Label`)                                           # Set as factor
  	
  	if (max(Linear_BU$`Linear Biodiversity Units`) == 0){
  	  maxY <- 5
  	} else {
  	  maxY <- max(Linear_BU$`Linear Biodiversity Units`)
  	}
  	
  	##### Create plot
  	print("Creating plot3...")
  	png(filename=plot3_path, width = 480, height = 480,
  	    units = "px", family="Lato")
  	showtext_begin()
  	par(mar = c(8,6.5,3,1), bg="#FFFFFF")                                                           # Set plot extent
  	plot(Linear_BU$row_num, Linear_BU$`Linear Biodiversity Units`,
  	     xaxt="n",yaxt="n",xlab="",ylab="")                                         # Create temporary extent
  	rect(par("usr")[1], par("usr")[3],
  	     par("usr")[2], par("usr")[4],
  	     col = "#E4ECED")                                                           # Color background
  	par(new = TRUE)                                                                 # Add new plot
  	plot(Linear_BU$row_num, Linear_BU$`Linear Biodiversity Units`, las=1,cex.axis=1.2,
  	     ylab = "", pch=16, col='#2b444b',col.ticks = "#2b444b",
  	     xlab = '',xaxt = 'n',col.axis="#2b444b", ylim=c(0,maxY),
  	     cex=1.5, main="Biodiversity units - Linear habitats", col.main="#2b444b")                                          # Create plot
  	axis(side=1, at=Linear_BU$row_num, labels = Sites, cex.axis=1.2,las=2,col.axis="#2b444b", col.ticks = "#2b444b")    # Add axis
  	title(ylab = "Biodiversity Units", cex.lab = 1.2,line = 4,
  	      col.lab = '#2b444b')                                                       # ylabel
  	box(col="#2b444b")                                                              # Assign box and colour
  	# Saved as plot 3
  	showtext_end()
  	dev.off()
  	
  	####################################### Non-BM habitats
  	##### Prepare data
  	qNon_BM <- sprintf("SELECT r.non_biodiv_habitats, q.aoi_label FROM public.processing_results r JOIN public.processing_queue q ON r.site_id = q.id 
      WHERE r.batch_id = '%s' ORDER BY q.aoi_label ASC;", batch_id)
  	Non_BM_JSON <- dbGetQuery(dbConnection, qNon_BM)
  	Non_BM <- jsonToDf(Non_BM_JSON$non_biodiv_habitats)

  	Non_BM <- base::merge(Non_BM, Options, by.x = c('Id'),by.y = c('id'),             
  	                all.x = TRUE, all.y = TRUE)                              # Add missing options!
  	Non_BM <- subset(Non_BM, select=c(`Site Label`,Habitat,`Area of habitat in site (ha)`))     # Simplify table
  	Non_BM <- Non_BM %>% group_by(`Site Label`, Habitat) %>%
  	  summarise(`Area of habitat in site (ha)` = sum(`Area of habitat in site (ha)`))   # Group amount by Site and habitat group

  	# Create colour pallette
  	habitat <- c("Ancient Woodland","Deep peaty soils","Shallow peaty soils",
  	             "Shallow peaty pockets")
  	Colour <- c("#1C442C","#6DB8CA","#FBC740","#B85450")
  	Hab_colours <- data.frame(habitat,Colour,stringsAsFactors=FALSE)
  	r_col<-	  Non_BM[Non_BM$`Area of habitat in site (ha)` != 0,]

  	# Make wide
  	Non_BM_wide <- spread(Non_BM, key=`Site Label`, value=`Area of habitat in site (ha)`)                 # df long to wide
  	Hab_colours <- Hab_colours[(Hab_colours$habitat %in% r_col$Habitat),]     # Remove any colours for missing habitats
  	Non_BM_wide <- subset(Non_BM_wide, select=-c(Habitat))                          # Simplify table
  	Non_BM_wide[is.na(Non_BM_wide)] <- 0                                            # Replace NAs with 0 
  	Non_BM_wide <- Non_BM_wide[rowSums(Non_BM_wide[])>0,]                                    # remove rows with 0
  	colnames(Non_BM_wide) <- Options$aoi_label                                       # Assign Site_ID as column header
  	
  	##### Create plot
  	print("Creating plot4...")
  	png(filename=plot4_path,width = 750, height = 480,
  	    units = "px", family="Lato")
  	showtext_begin()
  	par(mar = c(8,6.5,3,20), xpd=T, bg="#FFFFFF")                                                # Set plot extent
  	if (max(Non_BM$`Area of habitat in site (ha)`)== 0){
  	  #if no habitats are present
  	  y <- rep(c(0),time=(nrow(Options)))
  	  plot(as.factor(Options$aoi_label),y,xaxt="n",yaxt="n",col.axis="#2b444b",
  	       ylab = "", xlab = '')                               # Create temporary extent
  	  rect(par("usr")[1], par("usr")[3],
  	       par("usr")[2], par("usr")[4],
  	       col = "#E4ECED")                                                           # Color background
  	  par(new = TRUE)
  	  plot(as.factor(Options$aoi_label),y, las=2,cex.axis=1.2,
  	        ylab = "", xlab = '', border=NA,col.axis = "#2b444b",
  	        col.lab = "#2b444b", col.ticks = "#2b444b",ylim=c(0,5),
  	        main= "Area of Non-Biodiversity Metric habitats", col.main="#2b444b")          
  	} else {
  	  barplot(as.matrix(Non_BM_wide),xaxt="n",yaxt="n",col.axis="#2b444b")                               # Create temporary extent
  	  rect(par("usr")[1], par("usr")[3],
  	       par("usr")[2], par("usr")[4],
  	       col = "#E4ECED")                                                           # Color background
  	  par(new = TRUE)                                                              
  	  barplot(as.matrix(Non_BM_wide),col=Hab_colours$Colour, las=2,cex.axis=1.2,
  	          ylab = "", xlab = '',border=NA,col.axis = "#2b444b",
  	          col.lab = "#2b444b",col.ticks = "#2b444b",
  	          main= "Area of Non-Biodiversity Metric habitats", col.main="#2b444b")          # Create plot
  	  legend('right', legend=Hab_colours$habitat, bty='n',                           # Assign box and colour
  	         col=Hab_colours$Colour, cex=1.2, pch=15,ncol=1,inset=c(-0.7,0),
  	         xpd=TRUE, text.col="#2b444b",pt.cex=2)                                   # Add legend
  	  }
  	title(ylab = "Area (ha)", cex.lab = 1.2, line = 4,col.lab = '#2b444b')            # y label
  	#title(xlab = "", cex.lab = 1.2, line = 5,col.lab = '#2b444b')              # x label
  	box(col="#2b444b")
  	# Saved as plot 4
  	showtext_end()
  	dev.off()


	} else {

  	####################################### Baseline area habitats - single sites

  	# Prepare data
  	Single_Habs <- jsonToDf(Baseline_HabsJSON$baseline_habitats_area)
  	# Only create plots if there's any intersecting habitat present on the site
  	if (dim(Single_Habs)[1]!=0){
  	  print("Creating plot5...")
  	  
    	png(filename=plot5_path, width = 750, height = 750,
    	    units = "px", family="Lato")
    	showtext_begin()
    	par(mar = c(27,6.5,3,20), xpd=TRUE, bg="#FFFFFF")                                                # Set plot extent
  	  Single_Habs <- with(Single_Habs,  Single_Habs[order(Habitat) , ])
  	  colours <- c("#14634a","#e5a33a","#b85450","#4294a3","#e38c61","#2b444b",
  	               "#dfc063","#307553","#e1e9ea","#d76c60","#a1dad5","#003d59",
  	               "#43666e","#abbf75","#d1909a","#6db8ca","#876b53","#06161a")
  	  Single_Habs$colours <- colours[1:nrow(Single_Habs)]
  	  barplot(Single_Habs$`Maximum area of habitat in site (ha)`~ Single_Habs$Habitat,
  	          xaxt="n",yaxt="n",col.axis="#2b444b",xlab="",ylab="")                        # Create temporary extent
  	  rect(par("usr")[1], par("usr")[3],
  	       par("usr")[2], par("usr")[4],
  	       col = "#E4ECED")                                                           # Color background
  	  par(new = TRUE)                                                                 # Add new plot
  	  barplot(Single_Habs$`Maximum area of habitat in site (ha)`~Single_Habs$Habitat,
  	          col=Single_Habs$colours, las=2,cex.axis=1.2,xaxt="n",
  	          ylab = "", xlab = '',border=NA,col.axis = "#2b444b", col.ticks = "#2b444b",
  	          col.lab = "#2b444b", main= "Area of habitats in the site", col.main="#2b444b")    # Create plot
  
    	title(ylab = "Area (ha)", cex.lab = 1.2,line = 4,col.lab = '#2b444b')             # ylabel
    	#title(xlab = '', cex.lab = 1.2,line = 5,col.lab = '#2b444b')               # xlabel
    	box(col="#2b444b")                                                              # Assign box and colour                                  # Add legend                                  # Add legend                                # Add legend
    	#saved as plot 5
    	if(dim(Single_Habs)[1] != 0){
    	  legend("bottomleft", 
    	         legend=Single_Habs$Habitat, 
    	         col=Single_Habs$colours, 
    	         pch = c(15), 
    	         bty = "n", 
    	         pt.cex = 2, 
    	         cex = 1.2, 
    	         text.col = "#2C444B", 
    	         ncol=1,
    	         inset = c(-0, -0.6),
    	         xpd=T)                                 # Add legend
    	}
    	showtext_end()
    	dev.off()
  
    	####################################### 
    	
    	##### Number of habitats in Distinctiveness group
    	
    	# Prepare data
  
  	  count <- Single_Habs %>% 
  	  group_by(Distinctiveness) %>%
  	  summarise(no_rows = length(Distinctiveness))
    	colour <- c("#14634a","#abbf75","#e5a33a","#d76c60","#6C2624")
    	count$Distinctiveness <- factor(count$Distinctiveness, levels=c('V.Low', 'Low', 'Medium', 'High', 'V.High'))
    	count <- na.omit(count[order(levels(count$Distinctiveness)),])
    	D_groups <- as.factor(c('V.Low', 'Low', 'Medium', 'High', 'V.High'))                                           # Set as factor
    	# Create plot
    	print("Creating plot6...")
    	png(filename=plot6_path, width = 480, height = 480,
    	    units = "px", family="Lato")
    	showtext_begin()
    	par(mar = c(8,6.5,3,2), xpd=TRUE, bg="#FFFFFF")                                                # Set plot extent
    	barplot(count$no_rows~count$Distinctiveness,xaxt="n",yaxt="n",
    	        col.axis="#2b444b",xlab="",ylab="")                        # Create temporary extent
    	rect(par("usr")[1], par("usr")[3],
    	     par("usr")[2], par("usr")[4],
    	     col = "#E4ECED")                                                           # Color background
    	par(new = TRUE)                                                                 # Add new plot
    	barplot(count$no_rows~count$Distinctiveness,col=colour, las=2,cex.axis=1.2,
    	          ylab = "", xlab = '',border=NA,col.axis = "#2b444b",
    	          col.ticks = "#2b444b",col.lab = "#2b444b",yaxt = "n",xaxt = "n",
    	          main= "Number of habitats in distinctiveness groups", col.main="#2b444b")    # Create plot
    	axis(2, at = 0:(max(count$no_rows)),las=2,cex.axis=1.2)
    	axis(side=1, at=c(0.6,1.9,3.1,4.3,5.5), cex.axis=1.2,labels = D_groups,las=2,col.axis="#2C444B", col.ticks = "#2C444B")       # Add axis
    	title(ylab = "Number of habitats", cex.lab = 1.2,line = 4,col.lab = '#2b444b')             # ylabel
    	box(col="#2b444b")                                                              # Assign box and colour                                  # Add legend                                  # Add legend                                # Add legend
    	#saved as plot 6
    	showtext_end()
    	dev.off()
    	}
	}
	}
dbDisconnect(dbConnection)
print("FINISHED")

###############################################################################

# End

###############################################################################








