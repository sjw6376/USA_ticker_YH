library(httr)
library(rvest)

# 함수
html_table_fix <- function(x, header = NA, trim = TRUE,
                           fill = FALSE, dec = ".") {
  
  stopifnot(html_name(x) == "table")
  
  # Throw error if any rowspan/colspan present
  rows <- html_nodes(x, "tr")
  n <- length(rows)
  cells <- lapply(rows, "html_nodes", xpath = ".//td|.//th")
  
  ncols <- lapply(cells, html_attr, "colspan", default = "1")
  # Replace empty values of colspan with "1"
  ncols <- lapply(ncols, function(x) {x[x==""] <- "1"; x})
  ncols <- lapply(ncols, as.integer)
  nrows <- lapply(cells, html_attr, "rowspan", default = "1")
  nrows <- lapply(nrows, as.integer)
  
  p <- unique(vapply(ncols, sum, integer(1)))
  maxp <- max(p)
  
  if (length(p) > 1 & maxp * n != sum(unlist(nrows)) &
      maxp * n != sum(unlist(ncols))) {
    # then malformed table is not parsable by smart filling solution
    if (!fill) { # fill must then be specified to allow filling with NAs
      stop("Table has inconsistent number of columns. ",
           "Do you want fill = TRUE?", call. = FALSE)
    }
  }
  
  values <- lapply(cells, html_text, trim = trim)
  out <- matrix(NA_character_, nrow = n, ncol = maxp)
  
  # fill colspans right with repetition
  for (i in seq_len(n)) {
    row <- values[[i]]
    ncol <- ncols[[i]]
    col <- 1
    for (j in seq_len(length(ncol))) {
      out[i, col:(col+ncol[j]-1)] <- row[[j]]
      col <- col + ncol[j]
    }
  }
  
  # fill rowspans down with repetition
  for (i in seq_len(maxp)) {
    for (j in seq_len(n)) {
      rowspan <- nrows[[j]][i]; colspan <- ncols[[j]][i]
      if (!is.na(rowspan) & (rowspan > 1)) {
        if (!is.na(colspan) & (colspan > 1)) {
          # special case of colspan and rowspan in same cell
          nrows[[j]] <- c(utils::head(nrows[[j]], i),
                          rep(rowspan, colspan-1),
                          utils::tail(nrows[[j]], length(rowspan)-(i+1)))
          rowspan <- nrows[[j]][i]
        }
        for (k in seq_len(rowspan - 1)) {
          l <- utils::head(out[j+k, ], i-1)
          r <- utils::tail(out[j+k, ], maxp-i+1)
          out[j + k, ] <- utils::head(c(l, out[j, i], r), maxp)
        }
      }
    }
  }
  
  if (is.na(header)) {
    header <- all(html_name(cells[[1]]) == "th")
  }
  if (header) {
    col_names <- out[1, , drop = FALSE]
    out <- out[-1, , drop = FALSE]
  } else {
    col_names <- paste0("X", seq_len(ncol(out)))
  }
  
  # Convert matrix to list to data frame
  df <- lapply(seq_len(maxp), function(i) {
    utils::type.convert(out[, i], as.is = TRUE, dec = dec)
  })
  names(df) <- col_names
  class(df) <- "data.frame"
  attr(df, "row.names") <- .set_row_names(length(df[[1]]))
  
  if (length(unique(col_names)) < length(col_names)) {
    warning('At least two columns have the same name')
  }
  
  df
}

fdata <- list()

industry <- c('basic_materials','communication_services','consumer_cyclical','consumer_defensive','energy','healthcare','industrials','real_estate','technology','utilities')

for (j in 1: 10) {
  assign(paste0('fdata',j),list())
}

for (j in 1:10){
  for (i in 0 : 4){ #4 이거 페이지 바꿔도 됨.
    url = paste0('https://finance.yahoo.com/screener/predefined/ms_',industry[j],'?count=100&offset=',i,'00')
    
    data <- read_html(url) %>% html_nodes(xpath='//*[@id="scr-res-table"]/div[1]/table') %>% html_table_fix()
    data <- data[,c(1,8)]
    fdata[[i+1]] <- data
    
    Sys.sleep(2)
  } 
  fdata <- do.call(rbind,fdata)
  assign(paste0('fdata',j),fdata)
  write.csv(fdata,paste0('data/',industry[j],'.csv'))
  fdata = list()
}

dir.create('data/')

ffdata <- rbind(fdata1,fdata2,fdata3,fdata4,fdata5,fdata6,fdata7,fdata8,fdata9,fdata10)
write.csv(ffdata,'data/total.csv')
