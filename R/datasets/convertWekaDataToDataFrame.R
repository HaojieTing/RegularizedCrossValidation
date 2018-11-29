#Convert the weka arff data to data.frame and save it

library(RWeka)
dirname <- '/public/swl/datasets/weka_datasets/classification'
outdir <- '~/wekacla'
filenames <- list.files(path = dirname)
for(filename in filenames) {
  abs_path_file <- file.path(dirname,filename)
  data <- read.arff(abs_path_file)
  print(paste(filename, nrow(data)))
  saveRDS(data, file=file.path(outdir, paste(filename, "df", sep=".")))
}