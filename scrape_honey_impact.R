
library(tidyverse)
library(rvest)
library(glue)

char_name_list <-
  read_html("https://genshin.honeyhunterworld.com/db/char/characters/?lang=EN") %>%
  html_nodes(".sea_charname") %>%
  html_text() %>%
  str_replace_all(" ", "") %>%
  str_replace("KamisatoAyaka", "Ayaka") %>%
  str_replace("Yanfei", "FeiYan") %>%
  str_replace("KaedeharaKazuha", "Kazuha") %>%
  str_replace("RaidenShogun", "Shougun") %>%
  str_replace("SangonomiyaKokomi", "Kokomi") %>%
  str_replace("KujouSara", "Sara")

char_name_list <- char_name_list[!(char_name_list == "Traveler")]

get_char_stats <- function(name = "Xiangling", lang = "EN") {
  loaded_page <- read_html(glue("https://genshin.honeyhunterworld.com/db/char/{name}/?lang={lang}"))
  loaded_tables <- loaded_page %>% html_nodes(".add_stat_table") %>% html_table(fill = TRUE)
  # ---------- base stats table ----------------------
  base_stats <- data.frame(loaded_tables[[2]][-1,1:7])
  names(base_stats) <- loaded_tables[[2]][1,1:7]

  base_stats <- base_stats %>%
    filter(!str_detect(Lv, "\\+")) %>%
    mutate(across(1:4, as.numeric)) %>%
    mutate(across(5:7, ~as.numeric(str_replace(.x, "\\%", ""))/100))

  names(base_stats)[5] <-
  base_stats[["ASC ATTR"]] <- names(base_stats)[5]
  base_stats[["ASC Rate"]] <- base_stats[,5]
  base_stats[,5] <- NULL

  # ---------- attack stats table ----------------------
  trans_plus <- Vectorize(function(x) { 
    str_replace_all(x, "\\%", "") %>% 
      str_replace_all("×|×|\\*", "*") %>% 
      parse(text = .) %>% 
      eval() %>% 
      as.character()
  })
  
  attack_stats <- data.frame(loaded_tables[[3]][-1,])
  names(attack_stats) <- c("name", loaded_tables[[3]][1,-1])
  attack_stats <- attack_stats %>%
    gather(Lv, `Base ATK Value`, -name) %>%
    rename("Base ATK ATTR" = "name") %>%
    mutate(Lv = as.numeric(str_replace(Lv, "Lv", ""))) %>%
    filter(`Base ATK ATTR` != "Low/High Plunge DMG") %>%
    mutate(`Base ATK Value` = ifelse(str_detect(`Base ATK Value`, "s"), 
                                     str_replace_all(`Base ATK Value`, "(\\/|s)", ""), 
                                     `Base ATK Value`)) %>% 
    mutate(`Base ATK Value` = ifelse(str_detect(`Base ATK Value`, "(\\+|x|×|\\*)"), 
                                     trans_plus(`Base ATK Value`),  
                                     `Base ATK Value`)) %>% 
    mutate(`Base ATK Value` = 
             case_when(str_detect(`Base ATK ATTR`, "DMG") ~ as.numeric(str_replace(`Base ATK Value`, "\\%", ""))/100,
                       TRUE ~ as.numeric(`Base ATK Value`)
                       )
           ) %>%
    mutate(across(contains("Hit DMG"), 
                  ~ifelse(is.na(`DMG per Star Jade`), 
                          `Normal Attack DMG`, .x))) %>% 
    spread(`Base ATK ATTR`, "Base ATK Value")

  return(list(base_stats = base_stats,
              attack_stats = attack_stats))
}

char_df_list <- map(char_name_list, possibly(get_char_stats, NULL))
names(char_df_list) <- char_name_list
for (i in c("1-Hit DMG","2-Hit DMG","3-Hit DMG", "4-Hit DMG")) {
  char_df_list[["Ningguang"]][[2]][[i]] <- 
    char_df_list[["Ningguang"]][[2]][[c("Normal Attack DMG")]] 
}

saveRDS(char_df_list, "char_df_list.Rds")

char_df_list %>% 
  imap_dfr(~.x[[2]] %>% 
             filter(Lv == 10) %>% 
             select(contains("-Hit")) %>% 
             mutate(name = .y) %>% 
             relocate(name)) %>% 
  # glimpse()
  ggplot(aes(x=`1-Hit DMG`, y=`3-Hit DMG`, col=name, label=name)) + 
  geom_point() + 
  geom_text() + 
  theme_minimal() + 
  theme(legend.position = "none")





