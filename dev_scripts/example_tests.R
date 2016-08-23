#test examples
library(tidyMultiAssay)
library(dplyr)
library(purrr)
df <- data_frame(function_name=ls("package:tidyMultiAssay")) %>%
        dplyr::mutate(class=map_chr(function_name, function(x) class(get(x))[1])) %>%
        dplyr::filter(class=='function')

map(df$function_name, function(x) example(x, package='tidyMultiAssay', character.only = TRUE))
