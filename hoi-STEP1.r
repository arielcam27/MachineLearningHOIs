
get_rK_single <- function(df_pars1, df_pars2, FUN = median) {

return(c( FUN(as.numeric(na.omit(df_pars1$r))),
          FUN(as.numeric(na.omit(df_pars1$K))),
          FUN(as.numeric(na.omit(df_pars2$r))),
          FUN(as.numeric(na.omit(df_pars2$K)))))
}