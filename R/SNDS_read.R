path_tables_snds <- function(table = 'DCIR', csv_ou_parquet = c('parquet','csv')){

  if(length(csv_ou_parquet)==2){csv_ou_parquet <- csv_ou_parquet[1]}

  paths <- if(csv_ou_parquet == 'parquet'){data_paths()$SNDS_parquet}else{data_paths()$SNDS_csv}

  paths <- if(csv_ou_parquet == 'parquet'){paths[grepl(paste0('/',table,'/'), paths)]}else{paths[grepl(paste0('/',table,'.*CSV'), paths)]}

  return(paths)
}
