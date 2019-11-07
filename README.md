# usefun
##  Useful functions

devtools::install_github("tati-micheletti/usefun")

### Making hexStickers

The functions to generate stickers are modulesAvailable() and moduleSticker(). An example of how to create the stickers is in the host repo (https://github.com/tati-micheletti/host/blob/master/stickers/generateStickers.R), but here is the main idea:
1. The table 'moduleTable' contains all information necessary for each one of the stickers (i.e. color scheme, image, etc.). This table is located in "https://github.com/tati-micheletti/host/raw/master/stickers/moduleTable.csv", and the sourced image file in https://github.com/tati-micheletti/host/tree/master/images. The images names need to match the image's names in the table.
2. To change a sticker follow the following:
  2.1. Change the parameters you are interested in changing in the table at https://github.com/tati-micheletti/host/blob/master/stickers/moduleTable.csv before downloading it, or in your local table after downloading it. If you modify your local copy of the table, please make sure you pass it to the function in 2.2.  
  2.2. Run the function `usefun::moduleSticker()`  
3. Re-adjust parameters as needed.
