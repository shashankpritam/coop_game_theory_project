library('plot.matrix')
library(gifski)
library(tensorr)


#Immigrant Variable Assigned
base_PTR = 0.12
random_coop_defect = list(0, 1)
random_color = list("blue", "black", "green", "yellow")

#Storage Dataframe Initialized
df <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(df) <- c('row', 'column', 'occupancy', 'tag1', 'tag2', 'tag3', 'PTR')

#The visual Matrix Created
Random_Matrix <- matrix(runif(2500),nrow=50)
Game_Matrix <- matrix(0, nrow = 50, ncol = 50)
Graph_Matrix <- matrix(nrow = 50, ncol = 50)
dim(Graph_Matrix)<-c(50,50)

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
  random_column = (sample((1:50), size=1, replace=TRUE))
  random_row = (sample((1:50), size=1, replace=TRUE))
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
      print(1)
    }else{
      print(0)
}}
  
  
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
      print(Game_Matrix[row,col])
      print(c(random_row, random_column))
      print(c(row, col))
    }
  {name = paste('Matrix_',i,'_plot.png', sep='')}
  png(name,width=9,height=7.5,units='in',res=400)
  par(mar=c(5.1, 4.1, 4.1, 4.1),pty='s')
  plot(Graph_Matrix, col=topo.colors, main = "Graph Matrix", xlab = "Cell", ylab = "Cell",)
  dev.off()  

  par(mar=c(5.1, 4.1, 4.1, 4.1),pty='s')
  plot(Graph_Matrix, col=topo.colors, main = "Graph Matrix", xlab = "Cell", ylab = "Cell",)
  
  for(idx in 1:50)
  {
    for(idy in 1:50)
    {
      print(c(idx, idy))
      Arow = idx
      Acolumn = idy
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
