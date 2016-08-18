#test examples
library(biotidyr)
library(dplyr)
library(purrr)
df <- data_frame(function_name=ls("package:biotidyr")) %>%
        dplyr::mutate(class=map_chr(function_name, function(x) class(get(x))[1])) %>%
        dplyr::filter(class=='function')

map(df$function_name, function(x) example(x, package='biotidyr', character.only = TRUE))
