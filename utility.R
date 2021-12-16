
if (!require("pacman")) install.packages("pacman")

pacman::p_load(forcats)
pacman::p_load(viridis)
pacman::p_load(RColorBrewer)
pacman::p_load(DT)
pacman::p_load(scales)
pacman::p_load(lazyeval)
pacman::p_load(htmlTable)
pacman::p_load(Hmisc)
pacman::p_load(broom)
pacman::p_load(gridExtra)
# pacman::p_load(ggalt)
pacman::p_load(ggrepel)
pacman::p_load(lubridate)
pacman::p_load(stringr)
pacman::p_load(openxlsx)
pacman::p_load(grid)
pacman::p_load(magrittr)
pacman::p_load(kableExtra)
pacman::p_load(tidyverse)

pacman::p_load(forecast)
pacman::p_load(lubridate)
pacman::p_load(gghighlight)
pacman::p_load(xts)

pacman::p_load(ggExtra)
pacman::p_load(GGally)
pacman::p_load(ggbeeswarm)
pacman::p_load(memoise)
pacman::p_load(clipr)

pacman::p_load(patchwork)
pacman::p_load(glue)

## For windows to use cairo in plotting
trace(grDevices::png, quote({
  if (missing(type) && missing(antialias)) {
    type <- "cairo-png"
    antialias <- "subpixel"
  }
}), print = FALSE)

datatable <- function(...) DT::datatable(style = "default", 
                                         filter = 'top',
                                         extensions = "Buttons", 
                                         class = "stripe compact hover row-border", 
                                         ...)

Nweekdays <- Vectorize(
  function(a, b) sum(!weekdays(seq(a, b, "days")) %in% c("Saturday", "Sunday"))
  )
Nweekends <- Vectorize(
  function(a, b) sum(weekdays(seq(a, b, "days")) %in% c("Saturday", "Sunday"))
  )

.match <- function(..., incomparables=NA) match(..., incomparables=incomparables)
.count <- function(df, ...) count(df, ...) %>% mutate(percent = n/sum(n)) %>% arrange(-n)

str_wrap_factor <- function(x, ...) {
  levels(x) <- str_wrap(levels(x), ...)
  x
}

rdf <- function(df, digits = 2) { 
  df %>% 
    map_if(is.numeric, ~round(.x, digits)) %>%
    do.call(bind_cols,.)
}

pdf <- function(df, digits = 2, width = 40, total = F, bd = F, ...) {
  df %>% 
    rdf(digits = digits) %>%
    # htmlTable::htmlTable(rnames = F,
    #                      ctable = c("solid", "double"),
    #                      align = "lc",
    #                      css.cell = paste0('padding: 0px ', width,'px 0px;white-space: nowrap;',
    #                                        ifelse(bd, "border: 1px solid #BEBEBE;", "")),
    #                      total = total, 
    #                      ...)
    knitr::kable("html", ...) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                  full_width = F)
}


write.csv <- function(...) utils:::write.csv(..., row.names = FALSE)
copy.table <- function(...) utils:::write.table(..., file = "clipboard-16384",row.names = F, sep = "\t")
copy.table2 <- function(...) clipr::write_clip(...)
read.copy <- function(...) read.table("clipboard", sep="\t", header = T) %>% as_tibble()
filter_in <- function(data,v1,v2) filter(data,as.logical(rowSums(sapply(data[v1],"%in%",v2))))
remove_na_columns <- function(df) Filter(function(x) !all(is.na(x)), df)

# wmean <- function(x, weight, ...) {
#   # n.rep <- length(x)/length(weight)
#   # if (!(n.rep %% 1 == 0)) stop("length of weight is not a multiple of the length of x")
#   
#   # weighted.mean(x, w = rep(weight, n.rep))
#   sum(x*weight/sum(weight, na.rm=T), na.rm=T)
# }

# wmean <- function(x, weight, ...) weighted.mean(x, w = weight)
q_25 <- function(x, ...) quantile(x, .25, ...)
q_75 <- function(x, ...) quantile(x, .75, ...)
ci_95 <- function(x, ...) paste0("+/-", round(100*smean.cl.boot(x)[3]/smean.cl.boot(x)[1] - 100), "%")
ci_95_n <- function(x, ...) paste0("+/-", round(100*smean.cl.normal(x)[3]/smean.cl.normal(x)[1] - 100), "%")

trim_num <- function(x, qtl = 0.99) {
  x[x>quantile(x, 1-qtl) & x<quantile(x, qtl)]
}

# DC <- c("#002776","#81BC00", 
#         "#00A1DE","#72C7E7",
#         "#3C8A2E", "#BDD203", 
#         "#8C8C8C")

DC <- c("#FF1C33", # 1 SCG RED
        "#2E0029", # 2 SCG DEEP PURPLE
        "#50D2DC", # 3 SCG BLUE
        "#F0F0EB", # 4 SCG LIGHT GREY
        "#593B80", # 5 SCG PURPLE
        "#FFF5D2", # 6 SCG YELLOW
        "#AAAAA5") # 7 SCG DARK GREY

ggl <- function(lp = "right", ld = "v", lt = F, pb = F, lj = "c", bg = "w", pf = "w", lc = "g",  ...) {
  theme_classic(...) + 
    theme(legend.title = element_text(colour = ifelse(lt, "#1A1A1A", NA)),
          legend.position = lp,
          legend.text = element_text(size = 9), 
          legend.key.size = unit(.6, "cm"),
          
          axis.line = element_blank(),
          axis.line.x = element_line(colour = "#333333"),
          axis.line.y = element_line(colour = "#333333"),
          axis.ticks.length =  unit(0.1, "cm"),
          axis.title = element_text(color = "#333333", face = "bold"), 
          
          legend.background =  element_rect(colour = NA, fill = NA),
          legend.direction = ifelse(is.character(lp) && lp %in% c("left", "right"), "vertical", 
                                    ifelse(is.character(lp) && lp %in% c("top", "bottom"), "horizontal", 
                                    switch(ld, h = "horizontal", v = "vertical"))),
          legend.justification = ifelse(is.numeric(lj), lj, 
                                                   switch(lj, c = "centre", l = "left", r = "right")), 
          
          panel.border = element_rect(linetype = "solid", fill=NA, 
                                      color=ifelse(pb, "black", "white")), 
          panel.background =   element_rect(fill = switch(pf, w = "#FFFFFF", g = "#f5f5f5"), colour = NA), 
          panel.grid = element_line(colour = switch(lc, w = "#FFFFFF", g = "#f5f5f5")),
          panel.grid.major = element_line(colour = switch(lc, w = "#FFFFFF", g = "#f5f5f5")),
          panel.grid.major.x = element_line(colour = switch(lc, w = "#FFFFFF", g = "#f5f5f5")),
          panel.grid.major.y = element_line(colour = switch(lc, w = "#FFFFFF", g = "#f5f5f5")),
          panel.grid.minor = element_blank(),
          
          strip.text.x = element_text(size = 9, colour = "#333333",margin = margin(8, 0, 8, 0)),
          strip.text.y = element_text(size = 9, colour = "#333333"),
          strip.background = element_rect(fill = "#FFFFFF", colour = "#363636"),
          strip.switch.pad.grid = unit(0.1, "cm"),
          strip.switch.pad.wrap = unit(0.1, "cm"),
          
          plot.background = element_rect(colour = switch(bg, w = "#FFFFFF", g = "#f5f5f5"), 
                                         fill = switch(bg, w = "#FFFFFF", g = "#f5f5f5")),
          plot.title = element_text(size = rel(1), colour = "#1A1A1A", face = "bold", vjust = 1),
          plot.margin = unit(c(0.35, 0.75, 0.3, 0.2), "cm")
          
          )
}

# ggplot(mtcars, aes(x=mpg, y=wt, col=factor(cyl))) + geom_point() + facet_wrap(~as.factor(cyl)) + ggl(bg = "g")

ggc <- function(fc = DC, ...) scale_color_manual(values = fc, na.value = DC[7], ...)
ggf <- function(fc = DC, ...) scale_fill_manual(values = fc, na.value = DC[7], ...)
ggx <- function(xl = comma, name = waiver(), pbn=5, ...) scale_x_continuous(breaks = pretty_breaks(pbn), 
                                                                            name = name,
                                                                            labels = xl, ...)
ggxd <- function(name = waiver(), date_breaks = waiver(), date_labels = waiver(), pbn=8, ...) scale_x_date(breaks = pretty_breaks(pbn), 
                                                                                    name = name, 
                                                                                    date_breaks = date_breaks, 
                                                                                    date_labels = date_labels)
ggy <- function(xl = comma, name = waiver(), pbn=5, ...) scale_y_continuous(breaks = pretty_breaks(pbn), 
                                                                            name = name,
                                                                            labels = xl, ...)
ggnl <- function(...) labs(..., x = "", y = "")
ggvxl <- function(ts = 8, ag = 90, vj = .5, hj = 1, ...) theme(..., axis.text.x = element_text(angle = ag, hjust = hj, vjust = vj, 
                                                                           size = unit(ts,"cm")))
ggvxl2 <- function(ts = 8, ag = 45, vj = 1, hj = 1, ...) theme(..., axis.text.x = element_text(angle = ag, hjust = hj, vjust = vj, 
                                                                                   size = unit(ts,"cm")))

cpt <- function(mlist, ex = "", ext = "") {
  mlist %>% 
    map(tidy) %>% 
    do.call(rbind, .) %>% 
    rownames_to_column("mod") %>% 
    arrange(mod) %>% 
    mutate(term = fct_inorder(term)) %>% 
    separate(mod, c("mod", "coef"),sep="\\.") %>% 
    filter(!(mod %in% ex)) %>% 
    filter(!(term %in% ext)) %>% 
    ggplot(aes(x=term, y=estimate, col=mod, group=mod)) + 
    geom_point(position=position_dodge(width=.5), size=1) + 
    geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error), 
                  alpha=.5, position=position_dodge(width=.5), width=.5) + 
    geom_hline(yintercept = 0, linetype=2) + 
    coord_flip() + 
    ylab("Coefficients") + xlab("")
}

makeplot_mosaic <- function(data, x, y, ...){
  xvar <- deparse(substitute(x))
  yvar <- deparse(substitute(y))
  mydata <- data[c(xvar, yvar)];
  mytable <- table(mydata);
  widths <- c(0, cumsum(apply(mytable, 1, sum)));
  heights <- apply(mytable, 1, function(x){c(0, cumsum(x/sum(x)))});
  
  alldata <- data.frame();
  allnames <- data.frame();
  for(i in 1:nrow(mytable)){
    for(j in 1:ncol(mytable)){
      alldata <- rbind(alldata, c(widths[i], 
                                  widths[i+1], 
                                  heights[j, i], 
                                  heights[j+1, i]));
    }
  }
  colnames(alldata) <- c("xmin", "xmax", "ymin", "ymax")
  
  alldata[[xvar]] <- rep(dimnames(mytable)[[1]], 
                         rep(ncol(mytable), nrow(mytable)));
  alldata[[yvar]] <- rep(dimnames(mytable)[[2]], nrow(mytable));
  
  ggplot(alldata, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)) + 
    geom_rect(color="white", aes_string(fill=yvar), alpha=.8) +
    # xlab(paste(xvar, "(count)")) + 
    # ylab(paste(yvar, "(proportion)")) +
    ggy(percent) + 
    xlab("") + ylab("") + 
    coord_flip()
}

waterfall <- function(df, offset=0.3) {
  
  pacman::p_load(ggplot2)
  pacman::p_load(scales)
  pacman::p_load(dplyr)
  
  ## Add the order column to the raw data frame and order appropriately
  df <- df %>% mutate(order = as.numeric(category)) %>% arrange(order)
  
  ## The last value needs to be negated so that it goes down to
  ## zero.  Throws a warning if the cumulative sum doesn't match.
  last.id <- nrow(df)
  df$value[last.id] <- -df$value[last.id]
  
  ## Calculate the cumulative sums
  df <- df %>% mutate(cs1 = cumsum(value))
  
  ## Throw a warning if the values don't match zero as expected
  final_value <- tail(df$cs1, 1)
  if (final_value != 0) {
    warning(sprintf("Final value doesn't return to 0.  %.2d instead.", final_value))
  }
  
  ## Calculate the max and mins for each category and sector
  df <- transform(df, min.val = c(0, head(cs1, -1)),
                  max.val = c(head(cs1, -1), 0))    
  df <- df %>% group_by(order, category, sector, value, cs1) %>%
    summarize(min = min(min.val, max.val), max = max(min.val, max.val))
  
  ## Create the lines data frame to link the bars
  lines <- df %>% group_by(order) %>% summarize(cs = max(cs1))
  lines <- with(lines, data.frame(x = head(order, -1),
                                  xend = tail(order, -1),
                                  y = head(cs, -1),
                                  yend = head(cs, -1)))
  
  
  ## Add the offset parameter
  df <- transform(df, offset = offset)
  
  ## Make the plot    
  gg <- ggplot() +
    geom_segment(data = lines, aes(x = x, y = y, xend = xend, yend = yend), linetype = "dashed")  +
    geom_rect(data = df, aes(xmin = order - offset,
                             xmax = order + offset, 
                             ymin = min,
                             ymax = max, fill = sector)) +
    scale_x_continuous(breaks = unique(df$order), labels = unique(df$category))
  
  return(gg)
}

ggring <- function(df, xvar, keySize = 6, textSize = 8.5) {
  
  xvar = deparse(substitute(xvar))
  
  if (xvar == "status") {
    colors <- DC[c(4,3,2,1)]
  } else {
    # colors <- brewer.pal(n_distinct(data2[,xvar]),"Set2")
    colors <- DC
  }
  
  coFreq <- df %>% 
    group_by_(xvar) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    mutate(fraction = n/sum(n), 
           ymax = cumsum(fraction), 
           ymin = c(0, head(ymax,-1)))
  
  coFreq %>%
    ggplot(aes(ymax=ymax, ymin=ymin, xmax=12, xmin=8)) +
    geom_rect(color="white", aes_string(fill = xvar)) +
    geom_text(data = filter(coFreq, fraction >= 0.01), 
              aes(label=paste0(round(fraction*100),"%"),x=13.5,y=(ymin+ymax)/2), size=3.5) +
    scale_fill_manual(values = colors, na.value=DC[7]) +
    coord_polar(theta="y") +
    xlim(c(1,13.5)) +
    theme_void()+
    theme(legend.position = c(.50, .51),
          legend.title = element_blank(),
          legend.key.size = unit(keySize,"mm"), 
          legend.text = element_text(size = unit(textSize,"mm")))
}

cts_summary <- function(x, gvar = NULL, digits = 3, graph_size = 10) {
  
  if (!is.null(gvar) & length(x) != length(gvar)) stop("length of x and grouping variable does not match")

  cts_table <- tibble(key = c("Min", "1st_q", "Med", "3rd_q", "Max", "Mean", "Sd", 
                              "Miss", "Obs", "Distinct"),
                      value = c(fivenum(x, na.rm = T), mean(x, na.rm = T), sd(x, na.rm = T), 
                                sum(is.na(x)), length(x), n_distinct(x))) %>% rdf(digits)

  if (!is.null(gvar)) dd <- tibble(x=x, gvar=gvar)
  else dd <- tibble(x)
  
  cts_plot <- dd %>% 
    ggplot(aes(x)) + 
    geom_histogram(col = "white", bins = 30, fill = "#A3A3A3") + 
    ggy(comma, "") + 
    ggx(comma, "") + 
    ggl(base_size = graph_size)
  
  if (!is.null(gvar)) {
    dd_gmean <-  dd %>% 
      group_by_("gvar") %>% 
      summarise(x = mean(x, na.rm=T)) %>% 
      mutate(label = paste0(gvar, ": ", round(x, digits)))
    
    cts_plot <- cts_plot + 
      geom_vline(data = dd_gmean, aes(xintercept = x), 
                 col = "red", linetype = 2) + 
      facet_wrap(~gvar, labeller = labeller(gvar = setNames(dd_gmean$label, dd_gmean$gvar)))
  }
  
  list(cts_table, cts_plot)
}

dst_summary <- function(x, gvar = NULL, graph_size = 10) {
  
  if (!is.null(gvar) & length(x) != length(gvar)) stop("length of x and grouping variable does not match")
  
  if (!is.null(gvar)) {
    dd <- table(x, gvar, useNA = "ifany") %>% 
      data.frame() %>% 
      setNames(c("Category", "gvar", "Count"))
  } else {
    dd <- table(x, useNA = "ifany") %>% 
      data.frame() %>% 
      setNames(c("Category", "Count"))
  }
  
  dst_table <-  dd %>% 
    mutate(Category = as.character(Category)) %>% 
    mutate(Category = ifelse(is.na(Category), "missing", Category)) %>% 
    mutate(Percent = paste0(round(100*Count/sum(Count), 1), "%")) %>% 
    arrange(-Count) %>% 
    mutate(Category = str_wrap(Category, 30))
  
  dst_plot <- ggplot(dst_table, aes(fct_relevel(reorder(Category, Count), "missing"), Count)) + 
    geom_col(col = "white", fill = "#A3A3A3") + 
    ggy(comma, "") + 
    xlab("") + 
    coord_flip() +
    ggl(base_size = graph_size) + 
    theme(axis.text.y = element_text(size = unit(graph_size - 2, "cm")))
  
  if (!is.null(gvar)) dst_plot <- dst_plot + facet_wrap(~gvar)
  
  list(dst_table, dst_plot)
}

tlsummary <- function(df, gvar = NULL, mode = 'graph', export = F, 
                      graph_size = 10, table_size = 8, table_padding = c(2.3, 1.8)) {
  # browser()
  cts_names <- names(df)[map_lgl(df, ~is.numeric(.x) | is.integer(.x))]
  dst_names <- names(df)[map_lgl(df, ~is.character(.x) | is.factor(.x))]
  df_names <- names(df)[names(df) %in% c(cts_names, dst_names)]
  
  cts_plot_new <- suppressWarnings(map2(map(dplyr::select(df, cts_names), 
                                            ~cts_summary(.x, gvar = gvar, graph_size = graph_size)[[2]]), 
                                        str_wrap(cts_names, 80), ~.x + ggtitle(.y)))
  
  dst_plot_new <- suppressWarnings(map2(map(dplyr::select(df, dst_names),
                                            ~dst_summary(.x, gvar = gvar, graph_size = graph_size)[[2]]), 
                                        str_wrap(dst_names, 80), ~.x + ggtitle(.y)))
  
  if (mode == 'graph') {
    to_print <- suppressWarnings(
      map2(c(cts_plot_new,dst_plot_new)[df_names], 
           c(map(dplyr::select(df, cts_names), ~cts_summary(.x)[[1]]), 
             map(dplyr::select(df, dst_names), ~dst_summary(.x)[[1]]))[df_names], 
           ~grid.arrange(.x, tableGrob(.y, rows = NULL, cols = NULL, 
                                       theme = ttheme_minimal(base_size = table_size, 
                                                              padding = unit(table_padding, "mm"))), 
                         widths = c(1.5,1)))
    )
  } else if (mode == 'table') {
    c(map(dplyr::select(df, cts_names), ~cts_summary(.x)[[1]]), 
      map(dplyr::select(df, dst_names), ~dst_summary(.x)[[1]]))[df_names] %>% 
      map(~pdf(.x, 2))
  } else {stop("mode must be either 'graph' or 'table'")}
  
  if (export) to_print
}

# tlsummary(iris)



