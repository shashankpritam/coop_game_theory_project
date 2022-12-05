library('plot.matrix')
library(gifski)
library(tensorr)
library('dplyr')

#Immigrant Variable Assigned
base_PTR = 0.12
cost = 0.01
benefit = 0.03
mutation_rate = 0.005
cell_size = 50
matrix_size = cell_size*cell_size
random_coop_defect = list(0, 1)
random_color = list("blue", "black", "green", "yellow")

char_zero <- character(0)

#The visual Matrix Created
Random_Matrix <- matrix(runif(matrix_size),nrow=cell_size)
Game_Matrix <- matrix(0, nrow = cell_size, ncol = cell_size)
Graph_Matrix <- matrix(nrow = cell_size, ncol = cell_size)
dim(Graph_Matrix)<-c(cell_size, cell_size)

#Transition Storage Dataframe (For all Generation) Initialized
tsdf <- data.frame(matrix(ncol = 21, nrow = 0))
colnames(tsdf) <- c('Arow', 'Acolumn', 'AO', 
                    'NNrow','NNcolumn', 'NNO','NNI',
                    'NSrow', 'NScolumn', 'NSO', 'NSI',
                    'NErow', 'NEcolumn', 'NEO', 'NEI',
                    'NWrow', 'NWcolumn', 'NWO', 'NWI', 
                    'BasePTR', 'NewPTR')

i <- 0
gen <- 2
while (i < gen)
{
  # Temporary Storage Dataframe (For One Generation) Initialized
  df <- data.frame(matrix(ncol = 7, nrow = 0))
  colnames(df) <- c('row', 'column', 'occupancy', 'tag1', 'tag2', 'tag3', 'PTR')
  
  
  setwd('/Users/shashankpritam/Documents/qb_project')
  # Setting Up Immigrant Tags
  immigrant_tag_1_color = sample(random_color, 1, replace=TRUE)
  immigrant_tag_2_same_color = sample(random_coop_defect, 1, replace=TRUE)
  immigrant_tag_3_diff_color = sample(random_coop_defect, 1, replace=TRUE)
  
  #Immigrant Placement in the Dataframe Begins
  random_column = (sample((1:cell_size), size=1, replace=TRUE))
  random_row = (sample((1:cell_size), size=1, replace=TRUE))


  ## Check if the cell is already occupied, if not, continue
  new_im = paste(random_row, random_column, 1, sep=" ")
  if (new_im %in% paste(df$row, df$column, df$occupancy, sep=" ") == FALSE)
  {
  df[nrow(df) + 1,] <- c(random_row, random_column, 1, immigrant_tag_1_color, 
                         immigrant_tag_2_same_color, immigrant_tag_3_diff_color, 
                         base_PTR)

  
  #Immigrant Placement in the Matrix Begins
  # if tag = 1, it implies defection or non-cooperation
     if (immigrant_tag_2_same_color == 0 & immigrant_tag_3_diff_color == 0 ) {
      Graph_Matrix[random_row, random_column] <- "Humanitarian"
    } else if (immigrant_tag_2_same_color == 0 & immigrant_tag_3_diff_color == 1 ) {
      Graph_Matrix[random_row, random_column] <- "Ethnocentric"
    } else if (immigrant_tag_2_same_color == 1 & immigrant_tag_3_diff_color == 0 ) {
      Graph_Matrix[random_row, random_column] <- "Traitor"
    } else if (immigrant_tag_2_same_color == 1 & immigrant_tag_3_diff_color == 1 ) {
      Graph_Matrix[random_row, random_column] <- "Selfish"
    } else {
      Graph_Matrix[random_row, random_column] <- "Null"
    }
  
  
  {name = paste('Matrix_',i,'_plot.png', sep='')}
  png(name,width=9,height=7.5,units='in',res=400)
  par(mar=c(5.1, 4.1, 4.1, 4.1),pty='s')
  plot(Graph_Matrix, col=topo.colors, main = "Graph Matrix", xlab = "Cell", ylab = "Cell",)
  dev.off()  

  par(mar=c(5.1, 4.1, 4.1, 4.1),pty='s')
  plot(Graph_Matrix, col=topo.colors, main = "Graph Matrix", xlab = "Cell", ylab = "Cell",)
  
  ## Various Functions for Data Access
  
  ## Function to return occupancy of a cell
  occupancy.function <- function(x, y) {
    loc = paste(x, y, 1, sep=" ")
    if (loc %in% paste(df$row, df$column, df$occupancy, sep=" ") == TRUE){
      return (1)
    } else {
      return (0)
    }
  }
  
  ## Function to return tags of cell
  tags.function <- function(x, y) {
    return(filter(df, row == x & column == y & occupancy == 1))}
  
  ## Function to return the color of cell
  color.function <- function (x ,y){
    return(filter(df, row == x & column == y & occupancy == 1)$tag1)
  }
  
  # Start Roaming the Matrix by Checking Occupancy one by one
  for(elements in rownames(df)){
    Arow = (df[elements, "row"])
    Acolumn = (df[elements, "column"])
    if (Arow == cell_size){
      ANSrow = 1
      ANScolumn = Acolumn
    } else {
      ANSrow = Arow+1
      ANScolumn = Acolumn}
    
    if (Acolumn == 1){
      ANWrow = Arow
      ANWcolumn = cell_size
    } else {
      ANWrow = Arow
      ANWcolumn = Acolumn-1}
    
    if (Arow == 1){
      ANNrow = cell_size
      ANNcolumn = Acolumn
    } else {
      ANNrow = Arow-1
      ANNcolumn = Acolumn}
    
    if (Acolumn == cell_size){
      ANErow = Arow
      ANEcolumn = 1
    } else {
      ANErow = Arow
      ANEcolumn = Acolumn+1}
    
    #print(c(Arow, Acolumn, ANEcolumn))
    
    AO = occupancy.function(Arow, Acolumn)
    SO = occupancy.function(ANSrow, ANScolumn)
    WO = occupancy.function(ANWrow, ANWcolumn)
    NO = occupancy.function(ANNrow, ANNcolumn)
    EO = occupancy.function(ANErow, ANEcolumn)
    
    #print(c(AO, SO, WO, NO, EO))
    
    
    CAT = color.function(Arow, Acolumn)
    CST = color.function(ANSrow, ANScolumn)
    CWT = color.function(ANWrow, ANWcolumn)
    CNT = color.function(ANNrow, ANNcolumn)
    CET = color.function(ANErow, ANEcolumn)
    
    #print(c(CAT, CST, CWT, CNT, CET))
    
    AT = tags.function(Arow, Acolumn)
    ST = tags.function(ANSrow, ANScolumn)
    WT = tags.function(ANWrow, ANWcolumn)
    NT = tags.function(ANNrow, ANNcolumn)
    ET = tags.function(ANErow, ANEcolumn)
    
    
    interaction.function <- function(Atag1 = "Atag1", Atag2 = "Atag2", Atag3 = "Atag3",  
                                     Ntag1 = "Ntag1", Ntag2 = "Ntag2", Ntag3 = "Ntag3"){
      if(isTRUE((Atag1 == Ntag1) & (Atag2 == 0))){
        return (c(cost, benefit))
        
      } else if(isTRUE((Atag1 != Ntag1) & (Atag3 == 0))){
        return (c(cost, benefit))
        
      } else {
        return(c(0, 0))
        
      }}
    
    South_ID = c(CAT, AT$tag2, AT$tag3,  CST, ST$tag2, ST$tag3)
    West_ID = c(CAT, AT$tag2, AT$tag3,  CWT, WT$tag2, WT$tag3)
    North_ID = c(CAT, AT$tag2, AT$tag3,  CNT, NT$tag2, NT$tag3)
    East_ID = c(CAT, AT$tag2, AT$tag3,  CET, ET$tag2, ET$tag3)
    
    #if (identical(char_zero, (c(South_ID, West_ID, North_ID, East_ID)) == FALSE)) {
      NSI = interaction.function(South_ID)
      NWI = interaction.function(West_ID)
      NNI = interaction.function(North_ID)
      NEI = interaction.function(East_ID)
    #}
    
    
    #if (!is.null(c(NSI, NWI, NNI, NEI))){
      print(c(NSI, NWI, NNI, NEI))
    #}
    
    #tsdf[nrow(tsdf) + 1,] <- c(Arow, Acolumn, AO, 
    #                           ANSrow,ANScolumn, SO, NSI,
    #                          ANWrow, ANWcolumn, WO, NWI,
    #                          ANNrow, ANNcolumn, NO, NNI,
    #                          ANErow, ANEcolumn, EO, NEI, 
    #                          base_PTR, 'NewPTR') 
    #
    
  }
  }
i = i+1
}


print(df)
png_files <- list.files("/Users/shashankpritam/Documents/qb_project", pattern = ".*png$", full.names = TRUE)
#gifski(png_files, gif_file = "matrix_animation.gif", width = 1800, height = 1500, delay = 1)
invisible(file.remove(list.files(pattern = "*.png")))

#Result
#print(Graph_Matrix)

dims <- c(2500,16,gen)

## Tensor Storage
