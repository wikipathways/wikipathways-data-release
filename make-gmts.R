# generates GMT files from monthly data release content
library(rWikiPathways)
library(dplyr)

##############
# FUNCTIONS #
############

read_tsv_genes <- function(tsv_file) {
  df <- read.table(tsv_file, header = TRUE, sep = "\t", fill=T, row.names = NULL)
  df <- na.omit(df[nzchar(df$NCBI.gene), which(names(df) == "NCBI.gene")])
  if (length(df)==0)
    return(NULL)
  df <- unlist(strsplit(df, ";"))
  df <- df[grep("^ncbigene:", df)]
  df <- sub("^ncbigene:(\\d+)$","\\1",df)
  return(df)
}

read_info_file <- function(info_file) {
  info_lines <- readLines(info_file)
  info_list <- setNames(lapply(strsplit(info_lines, ":"), function(x) trimws(paste(x[2:length(x)],collapse=":"))), lapply(strsplit(info_lines, ":"), function(x) trimws(x[1])))
  info_list$title <- gsub("_+"," ",info_list$title)
  info_list$latin <- gsub("_"," ",info_list$latin)
  return(as.data.frame(info_list))
}

################
# RUN PER DIR #
##############
gmt_dirs <- list.dirs(full.names = F, recursive = F)

for (g in gmt_dirs){
  print(g)
  info_files <- list.files(g, pattern = "info$")
  
  merged_df <- NULL
  for (i in info_files) {
    print(i)
    
    # Get the matching .tsv file
    h <- sub("\\.info","\\.tsv",i)

    info_df <- read_info_file(file.path(g,i))
    gene_list <- read_tsv_genes(file.path(g,h))
    
    # If any NCBI genes, then...
    if(length(gene_list)>0){
      df <- cbind(info_df, gene=gene_list)
      # Append df of pathway to merged df
      merged_df <- rbind(merged_df, df)
    }
  }
  # Generate GMT
  if (!is.null(merged_df))
    rWikiPathways::writeGMT(merged_df, paste(g,"gmt", sep = "."))
}
