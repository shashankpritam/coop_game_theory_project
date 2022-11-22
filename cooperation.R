library(tensorr)
library('plot.matrix')

#Immigrant Variable Assigned
base_PTR = 0.12
random_coop_defect = list(0, 1)
random_color = list("blue", "black", "green", "yellow")
immigrant_tag_1_color = sample(random_color, 1, replace=TRUE)
immigrant_tag_2_same_color = sample(random_coop_defect, 1, replace=TRUE)
immigrant_tag_3_diff_color = sample(random_coop_defect, 1, replace=TRUE)
print(immigrant_tag_1_color)
print(immigrant_tag_2_same_color)
print(immigrant_tag_3_diff_color) 

#Storage Dataframe Initialized
df <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(df) <- c('row', 'column', 'occupancy', 'tag1', 'tag2', 'tag3', 'PTR')

#The visual Matrix Created
Random_Matrix <- matrix(runif(2500),nrow=50)
Game_Matrix <- Random_Matrix
Graph_Matrix <- matrix(nrow = 50, ncol = 50)
dim(Graph_Matrix)<-c(50,50)

#Immigrant Placement in the Dataframe Begins
random_column = (sample((1:50), size=1, replace=TRUE))
random_row = (sample((1:50), size=1, replace=TRUE))
print(random_row)
print(random_column)

df[nrow(df) + 1,] <- c(random_row, random_column, 1, immigrant_tag_1_color, immigrant_tag_2_same_color, 
                       immigrant_tag_3_diff_color, base_PTR)
print(df)

#Immigrant Placement in the Matrix Begins
for(row in 1:nrow(Game_Matrix)) {
  for(col in 1:ncol(Game_Matrix)) {
    if(identical(c(row, col), c(random_row, random_column)) == 'False') {
      Graph_Matrix[row, col] <- "00"
    } else if ((identical(c(row, col), c(random_row, random_column)) == 'True') &
               immigrant_tag_2_same_color == 0 & immigrant_tag_3_diff_color == 0 ) {
      Graph_Matrix[row, col] <- "HM"
    } else if ((identical(c(row, col), c(random_row, random_column)) == 'True') &
               immigrant_tag_2_same_color == 0 & immigrant_tag_3_diff_color == 1 ) {
      Graph_Matrix[row, col] <- "OK"
    } else if ((identical(c(row, col), c(random_row, random_column)) == 'True') &
               immigrant_tag_2_same_color == 1 & immigrant_tag_3_diff_color == 0 ) {
      Graph_Matrix[row, col] <- "ET"
    } else if ((identical(c(row, col), c(random_row, random_column)) == 'True') &
               immigrant_tag_2_same_color == 1 & immigrant_tag_3_diff_color == 1 ) {
      Graph_Matrix[row, col] <- "TR"  
    } else {
      Graph_Matrix[row, col] <- "PL"
    }
  }
}

#Result
#print(Graph_Matrix)

par(mar=c(5.1, 4.1, 4.1, 4.1))
plot(Graph_Matrix)

