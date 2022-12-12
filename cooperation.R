#@Shashank Pritam (shashankpritam@gmail.com)
#A simulation (of below cited article) written in R 
#Hammond, Ross A., and Robert Axelrod. “The Evolution of Ethnocentrism.” 
#Journal of Conflict Resolution, vol. 50, no. 6, Dec. 2006, pp. 926–36. DOI.org (Crossref), 
#https://doi.org/10.1177/0022002706293470.

# Libraries Needed
library('plot.matrix')
library(gifski)
library('dplyr')


# ------------------      All the global variables here    ---------------------
base_PTR = 0.12
cost = 0.01
benefit = 0.03
mutation_rate = 0.005
cell_size = 50
gen = 50
matrix_size = cell_size*cell_size
random_coop_defect = list(0, 1)
random_color = list("blue", "black", "green", "yellow")


## ---------------------     All the Functions here     ------------------------

## Various Functions for Data Access

## Function to return occupancy of a cell
## --------------------------------------
occupancy.function <- function(x, y) {
  loc = paste(x, y, 1, sep=" ")
  if (loc %in% paste(df$row, df$column, df$occupancy, sep=" ") == TRUE){
    return (1)
  } else {
    return (0)
  }
}

## Function to return tags of cell
## --------------------------------------
tags.function <- function(x, y) {
  return(filter(df, row == x & column == y & occupancy == 1))}

## Function to return the color of cell
## --------------------------------------
color.function <- function (x ,y){
  color = filter(df, row == x & column == y & occupancy == 1)$tag1
  if (identical(color, character(0)) == TRUE){
    color = "Empty"
    return (color)
  } else {
    return (color)
  }
}


## Function to manipulate PTR values based on all interactions
## -----------------------------------------------------------
interaction.function <- function(Nrow = NULL, Ncolumn = NULL, 
                                 Atag1 = "A", Atag2 = NULL, 
                                 Atag3 = NULL, Ntag1 = "N"){
  
  nbr_location = which((df$row == Nrow) & (df$column == Ncolumn))
  cell_location = which(df$row == Arow & df$column == Acolumn)
  Original_APTR = as.numeric(noquote(df[cell_location, 7]))
  Original_NPTR = as.numeric(noquote(df[nbr_location, 7]))
  
  if (identical(nbr_location, integer(0)) == FALSE){
    if(isTRUE((Atag1 == Ntag1) & (Atag2 == 0)  )){
      df[cell_location, 7] <<- Original_APTR  - cost
      df[nbr_location, 7] <<- Original_NPTR + benefit
      #print(c(Atag1, Ntag1, Nrow, Ncolumn, Atag2, Atag3, 
      #        df[cell_location, 7], df[nbr_location, 7], "Case 1"))
      
    } else if(isTRUE((Atag1 != Ntag1) & identical(Atag3, 0) )){
      df[cell_location, 7] <<- Original_APTR  - cost
      df[nbr_location, 7] <<- Original_NPTR + benefit
      #print(c(Atag1, Ntag1, Nrow, Ncolumn, Atag2, Atag3,
      #        df[cell_location, 7], df[nbr_location, 7], "Case 2"))
      
      
    } else {
      whatever = 1
      
    }}}

# -----------------          END OF FUNCTIONS        ---------------------------


# -------------------       Storage Management       ---------------------------

# The visual Matrix Created
## ------------------------
Random_Matrix <- matrix(runif(matrix_size),nrow=cell_size)
Game_Matrix <- matrix(0, nrow = cell_size, ncol = cell_size)
Graph_Matrix <- matrix(nrow = cell_size, ncol = cell_size)
dim(Graph_Matrix)<-c(cell_size, cell_size)

# Transition Storage Dataframe (For all Generation) Initialized
## ------------------------------------------------------------
tsdf <- data.frame(matrix(ncol = 8, nrow = 0))
colnames(tsdf) <- c('row', 'column', 'occupancy', 'tag1', 'tag2', 'tag3', 'PTR', 'gen')


# Temporary Storage Dataframe (For One Generation) Initialized
## -----------------------------------------------------------
df <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(df) <- c('row', 'column', 'occupancy', 'tag1', 'tag2', 'tag3', 'PTR')

# -------------------     End of Storage Management       ----------------------




# ------------------------- Simulation Starts Here -----------------------------
i <- 0
while (i < gen)
{
  
  
  #It's important to change the working directory before running of different system
  setwd('/Users/shashankpritam/Documents/qb_project')
  
  # Setting Up Immigrant Tags
  immigrant_tag_1_color = sample(random_color, 1, replace=TRUE)
  immigrant_tag_2_same_color = sample(random_coop_defect, 1, replace=TRUE)
  immigrant_tag_3_diff_color = sample(random_coop_defect, 1, replace=TRUE)
  
  #Immigrant Placement in the Dataframe Begins
  random_column = (sample((1:cell_size), size=1, replace=TRUE))
  random_row = (sample((1:cell_size), size=1, replace=TRUE))
  
  
  ## Check if the cell is already occupied, if not, continue from here
## ------------------------ Every gen starts here ------------------------------
  new_im = paste(random_row, random_column, 1, sep=" ")
  if (new_im %in% paste(df$row, df$column, df$occupancy, sep=" ") == FALSE)
  {
    df[nrow(df) + 1,] <- c(random_row, random_column, 1, immigrant_tag_1_color, 
                           immigrant_tag_2_same_color, immigrant_tag_3_diff_color, 
                           base_PTR)
    
## ------------------------Immigration starts here -----------------------------
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
    

    
    # Output - Dataframe Updated with new immigrant append at the Queue 
##--------------------- Interaction and PTR Update -----------------------------    
    # Start Roaming the Matrix by Checking Occupancy one by one
    for(elements in rownames(df)){

      Arow = as.integer(noquote(df[elements, "row"]))
      Acolumn = as.integer(noquote(df[elements, "column"]))
      
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
      
      
      CAT = color.function(Arow, Acolumn)
      CST = color.function(ANSrow, ANScolumn)
      CWT = color.function(ANWrow, ANWcolumn)
      CNT = color.function(ANNrow, ANNcolumn)
      CET = color.function(ANErow, ANEcolumn)
      
      AT = tags.function(Arow, Acolumn)
      ST = tags.function(ANSrow, ANScolumn)
      WT = tags.function(ANWrow, ANWcolumn)
      NT = tags.function(ANNrow, ANNcolumn)
      ET = tags.function(ANErow, ANEcolumn)
      
      South_ID = c(ANSrow, ANScolumn, CAT, AT$tag2, AT$tag3,  CST)
      West_ID = c(ANWrow, ANWcolumn, CAT, AT$tag2, AT$tag3,  CWT)
      North_ID = c(ANNrow, ANNcolumn, CAT, AT$tag2, AT$tag3,  CNT)
      East_ID = c(ANErow, ANEcolumn, CAT, AT$tag2, AT$tag3,  CET)
      
      NSI = interaction.function(Nrow = ANSrow, Ncolumn = ANScolumn, CAT, AT$tag2, AT$tag3,  CST)
      NWI = interaction.function(Nrow = ANWrow, Ncolumn = ANWcolumn, CAT, AT$tag2, AT$tag3,  CWT)
      NNI = interaction.function(Nrow = ANNrow, Ncolumn = ANNcolumn, CAT, AT$tag2, AT$tag3,  CNT)
      NEI = interaction.function(Nrow = ANErow, Ncolumn = ANEcolumn, CAT, AT$tag2, AT$tag3,  CET)
      
      
    }
    # Expected Output - Data frame Updated with new PTR values ------------------
##--------------------- Birth and PTR realization ------------------------------
    
    for(elements in rownames(df)){

      Arow = as.integer(noquote(df[elements, "row"]))
      Acolumn = as.integer(noquote(df[elements, "column"]))
      
      current_ptr = (df[elements, "PTR"])
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
      
      AO = occupancy.function(Arow, Acolumn)
      SO = occupancy.function(ANSrow, ANScolumn)
      WO = occupancy.function(ANWrow, ANWcolumn)
      NO = occupancy.function(ANNrow, ANNcolumn)
      EO = occupancy.function(ANErow, ANEcolumn)
      
      CAT = color.function(Arow, Acolumn)
      AT = tags.function(Arow, Acolumn)
      
      
      if (current_ptr > 0.12){
        list_of_vacant_nbr = list()
        if (SO == 0){
          list_of_vacant_nbr = append(list_of_vacant_nbr, c(ANSrow, ANScolumn), after = length(x))
        } else if (WO == 0){
          list_of_vacant_nbr = append(list_of_vacant_nbr, c(ANWrow, ANWcolumn), after = length(x))
        } else if (NO == 0){
          list_of_vacant_nbr = append(list_of_vacant_nbr, c(ANNrow, ANNcolumn), after = length(x))
        } else if (EO == 0){
          list_of_vacant_nbr = append(list_of_vacant_nbr, c(ANErow, ANEcolumn), after = length(x))
        } else{
          some_value = 1
        }
        vacant_nbr = sample(list_of_vacant_nbr)
        
        # Setting Up Immigrant Tags
        # A case of no mutation
        progeny_tag_1_color = CAT
        progeny_tag_2_same_color = AT$tag2
        progeny_tag_3_diff_color = AT$tag3
        
        #progeny Placement in the Dataframe Begins
        progeny_row = vacant_nbr[[1]]
        progeny_column = vacant_nbr[[2]]
        
        df[nrow(df) + 1,] <- c(progeny_row, progeny_column, 1, progeny_tag_1_color, 
                               progeny_tag_2_same_color, progeny_tag_3_diff_color, 
                               base_PTR)
        
        }
    }
    
    # Expected Output - Data frame Updated with new progeny appended at 
    # empty cells neighboring the occupied cell when PTR is realized
    # the mutaion rate and parents tags to be utilized
##--------------------------   Random Death    ---------------------------------
    
    len_of_df = as.integer(noquote(nrow(df)))
    if (i >= 10){ #population has to be more than 10 be killed
      death_rate = len_of_df%/%10 #in percentage
      sample_subset_df = sample_n(df, death_rate) 
      df <- suppressMessages(anti_join(df, sample_subset_df))
    }
    
    # Expected Output - Data frame Updated with removal of random cell 
    # with some probability
 
      
         
##-------------------     End of a generation     ------------------------------    
    
    i = i+1
    df_append <- cbind(df, i)
    tsdf = rbind(tsdf,df_append)
  
    }
}

  

## Save gif
png_files <- list.files("/Users/shashankpritam/Documents/qb_project", pattern = ".*png$", full.names = TRUE)
gifski(png_files, gif_file = "matrix_animation.gif", width = 1800, height = 1500, delay = 1)
invisible(file.remove(list.files(pattern = "*.png")))



#------------------------      Result          ---------------------------------
print(tsdf)
write.csv(tsdf, "result.csv", row.names=TRUE)
#-------------------       End of Simulation           -------------------------

