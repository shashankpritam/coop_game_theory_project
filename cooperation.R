library(tensorr)
library('plot.matrix')

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

i <- 0
while (i < 2500)
{
  
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

  df[nrow(df) + 1,] <- c(random_row, random_column, 1, immigrant_tag_1_color, immigrant_tag_2_same_color, 
                       immigrant_tag_3_diff_color, base_PTR)
  #print(df)

  #Immigrant Placement in the Matrix Begins
     if (immigrant_tag_2_same_color == 0 & immigrant_tag_3_diff_color == 0 ) {
      Graph_Matrix[random_row, random_column] <- "HM"
    } else if (immigrant_tag_2_same_color == 0 & immigrant_tag_3_diff_color == 1 ) {
      Graph_Matrix[random_row, random_column] <- "OK"
    } else if (immigrant_tag_2_same_color == 1 & immigrant_tag_3_diff_color == 0 ) {
      Graph_Matrix[random_row, random_column] <- "ET"
    } else if (immigrant_tag_2_same_color == 1 & immigrant_tag_3_diff_color == 1 ) {
      Graph_Matrix[random_row, random_column] <- "TR"
    } else {
      Graph_Matrix[random_row, random_column] <- "00"
      print(Game_Matrix[row,col])
      print(c(random_row, random_column))
      print(c(row, col))
      jpeg("/ndsu_classes/foo%02d.jpg")
      my.plot(Graph_Matrix)
      dev.off()

    }

i = i+1}

make.mov <- function(){
  unlink("plot.mpg")
  system("convert -delay 0.5 plot*.jpg plot.mpg")
}

print(df)
#Result
#print(Graph_Matrix)

par(mar=c(5.1, 4.1, 4.1, 4.1))
plot(Graph_Matrix)


