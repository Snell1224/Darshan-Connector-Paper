## Process data 

All codes for data processing and plots were written in R.
Total number of records 15672166
Had to filter last line of some, which carried the total number

```
install.packages("dplyr", "tidyverse", "arrow")
```

Read all csv files and join them in an unique parquet file, so I could filter by
application and setup. Run all inside "analysis" directory
(At least 30GB memory is necessary)
```
Rscript pre_process.R
```

Separate files one per each application based on their job_id from the excel document.
It will generate files in CSV and in parquet mode, I will use parquet format for data analysis
```
mkdir ./../data/files-per-app/
Rscript separate_files.R
```

## Analyse data and generate plots
```
Rscript hacc_process.R
Rscript mpi_io_process.R
```