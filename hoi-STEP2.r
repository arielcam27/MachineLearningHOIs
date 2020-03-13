

get_aij_double <-  function(df_pars4, df_pars5, df_pars6, FUN=median) {
  
  return(c( FUN(as.numeric(na.omit(df_pars4$a12))),
            FUN(as.numeric(na.omit(df_pars4$a21))),
            FUN(as.numeric(na.omit(df_pars5$a12))),
            FUN(as.numeric(na.omit(df_pars5$a21))),
            FUN(as.numeric(na.omit(df_pars6$a12))),
            FUN(as.numeric(na.omit(df_pars6$a21)))))
}