library('plot.matrix')
library(gifski)
library(tensorr)
library('dplyr')

#Immigrant Variable Assigned
base_PTR = 0.12
random_coop_defect = list(0, 1)
random_color = list("blue", "black", "green", "yellow")
cell_size = 50
matrix_size = cell_size*cell_size

#Storage Dataframe Initialized
df <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(df) <- c('row', 'column', 'occupancy', 'tag1', 'tag2', 'tag3', 'PTR')

#The visual Matrix Created
Random_Matrix <- matrix(runif(matrix_size),nrow=cell_size)
Game_Matrix <- matrix(0, nrow = cell_size, ncol = cell_size)
Graph_Matrix <- matrix(nrow = cell_size, ncol = cell_size)
dim(Graph_Matrix)<-c(cell_size, cell_size)

#Transition Storage Dataframe
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
  setwd('/Users/shashankpritam/Documents/qb_project')
  immigrant_tag_1_color = sample(random_color, 1, replace=TRUE)
  immigrant_tag_2_same_color = sample(random_coop_defect, 1, replace=TRUE)
  immigrant_tag_3_diff_color = sample(random_coop_defect, 1, replace=TRUE)
  #print(immigrant_tag_1_color)
  #print(immigrant_tag_2_same_color)
  #print(immigrant_tag_3_diff_color)   
  #Immigrant Placement in the Dataframe Begins
  random_column = (sample((1:cell_size), size=1, replace=TRUE))
  random_row = (sample((1:cell_size), size=1, replace=TRUE))
  #print(random_row)
  #print(random_column)

## check if already occupied
  new_im = paste(random_row, random_column, 1, sep=" ")
  if (new_im %in% paste(df$row, df$column, df$occupancy, sep=" ") == FALSE)
  {
  df[nrow(df) + 1,] <- c(random_row, random_column, 1, immigrant_tag_1_color, immigrant_tag_2_same_color, 
                       immigrant_tag_3_diff_color, base_PTR)
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

  
  #print(df)
  
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
      #print(Game_Matrix[row,col])
      #print(c(random_row, random_column))
      #print(c(row, col))
    }
  {name = paste('Matrix_',i,'_plot.png', sep='')}
  png(name,width=9,height=7.5,units='in',res=400)
  par(mar=c(5.1, 4.1, 4.1, 4.1),pty='s')
  plot(Graph_Matrix, col=topo.colors, main = "Graph Matrix", xlab = "Cell", ylab = "Cell",)
  dev.off()  

  par(mar=c(5.1, 4.1, 4.1, 4.1),pty='s')
  plot(Graph_Matrix, col=topo.colors, main = "Graph Matrix", xlab = "Cell", ylab = "Cell",)
  
  
  for(idx in 1:cell_size)
  {
    for(idy in 1:cell_size)
    {
      Arow = idx
      Acolumn = idy
      AO = occupancy.function(Arow, Acolumn)
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
        
      print(c(Arow, Acolumn, ANEcolumn))
      SO = occupancy.function(ANSrow, ANScolumn)
      WO = occupancy.function(ANWrow, ANWcolumn)
      NO = occupancy.function(ANNrow, ANNcolumn)
      EO = occupancy.function(ANErow, ANEcolumn)
      
      NSO = tags.function(ANNrow, ANNcolumn)

    }
  }

i = i+1
}
}

print(df)
png_files <- list.files("/Users/shashankpritam/Documents/qb_project", pattern = ".*png$", full.names = TRUE)
#gifski(png_files, gif_file = "matrix_animation.gif", width = 1800, height = 1500, delay = 1)
invisible(file.remove(list.files(pattern = "*.png")))


#Result
#print(Graph_Matrix)




dims <- c(2500,16,gen)








## Tensor Storage
