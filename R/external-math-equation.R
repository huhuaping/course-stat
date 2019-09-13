
# function for lm model output
fun_lm_output<-function(lm.mod,lm.dt){
  #lm_mod<-lm(models$both_prod_log_f,table_demo1)
  #result<-(summary(lm_mod))
  lm_mod<-lm(lm.mod,lm.dt)
  result<-(summary(lm_mod))
  # table report of mostly used output 
  table_lm<-tibble(
    name.y=(names(attr(terms(lm_mod),"dataClasses")))[1],
    k=dim(result$coefficients)[[1]],
    n=dim(lm.dt)[1],  #change it when finished
    f.TSS = n - 1,                 # freedom of TSS
    f.RSS = n - k,             # freedom of RSS
    f.ESS = k - 1,             # freedom of ESS
    sigma.hat = result$sigma,
    R2 = result$r.squared,
    R2.adj = result$adj.r.squared,
    F.star = result$fstatistic[1],
    F.p = pf(result$fstatistic[1],
             result$fstatistic[2],result$fstatistic[3],
             lower.tail = F))
  return(table_lm)
}

# function for lm model series
fun_lm_series<-function(lm.mod,lm.dt){
  lm_mod<-lm(lm.mod,lm.dt)
  result<-(summary(lm_mod))
  
  series_lm<-tibble(
    Y.hat=lm_mod$fitted.values,
    residual=lm_mod$residuals,
    Y=Y.hat+residual) %>%
    dplyr::select(Y, Y.hat, residual)
  return(series_lm)
}

# function for lm model coefficents
fun_lm_coef<-function(lm.mod,lm.dt){
  lm_mod<-lm(lm.mod,lm.dt)
  result<-(summary(lm_mod))
  # matrix of coefficients
  mat_coef<-result$coefficients
  mat_coef<-as_tibble(mat_coef)%>%
    add_column(dimnames(mat_coef)[[1]],.before="Estimate") %>%
    setNames(c("vars","coef","ste","tstar","pvalue")) %>%
    
    mutate(coef_d2=formatC(abs(coef),digits = 2,format = "f")) %>%
    mutate(coef_sign=ifelse(coef>0, "+",
                            ifelse(coef==0,"","-"))) %>%
    mutate(ste_d4=formatC(ste,digits = 4,format = "f")) %>%
    mutate(tstar_d4=formatC(tstar,digits = 4,format = "f")) %>%
    mutate(pvalue_d4=formatC(pvalue,digits = 4,format = "f"))
  return(mat_coef)
}

# function for lm model variables
fun_lm_vars<-function(lm.mod,lm.dt){
  lm_mod<-lm(lm.mod,lm.dt)
  result<-(summary(lm_mod))
  # matrix of seperate variables completely
  mat_vars<- tibble(vars=unlist(dimnames(result$coefficients)[1]))
  mat_vars<- mat_vars %>%
    mutate(vars=dplyr::recode(vars,"(Intercept)" = "")) %>%
    separate(col=vars,into=c("left", "right"),
             sep=":",fill="right",extra = "merge",remove =F) %>%
    add_column(sep_mid=":",.before = "right") %>%
    separate(col=left,into=c("left_left", "left_right"),
             sep="_",fill="right",extra = "merge",remove =F) %>%
    add_column(sep_left="_",.before = "left_right") %>%
    separate(col=right,into=c("right_left", "right_right"),
             sep="_",fill="right",extra = "merge",remove =F) %>%
    add_column(sep_right="_",.before = "right_right") %>%
    dplyr::select(vars,left,right,                          # order the variabls
           left_left,sep_left,left_right,
           sep_mid,
           right_left,sep_right,right_right) %>%
    mutate(vars_left=str_c(left_left,"_{",           # latex for "_" variables
                           left_right,"}")) %>%      #   for non "_" variables
    mutate(vars_left_math=ifelse(is.na(left_right),
                                 left_left,
                                 vars_left)) %>%
    mutate(vars_right=str_c(right_left,"_{",
                            right_right,"}")) %>%     # latex for "_" variables
    mutate(vars_right_math=ifelse(is.na(right_right), #   for non "_" variables
                                  right_left,
                                  vars_right)) %>%
    mutate(vars_right_math=replace_na(vars_right_math,"")) %>% # replace NA
    mutate(sign_combine=ifelse(vars_right_math=="",            # obtain "*" 
                               "",
                               " \\ast ")) %>%
    mutate(vars_latex=str_c(vars_left_math,           # get "a*b"
                            sign_combine,
                            vars_right_math)) %>%     
    mutate(vars_latex=dplyr::recode(vars_latex,"(Intercept)"="")) %>% # treat "(Intercept)" 
    add_column(align_eq="&&") %>%
    add_column(par_pop=str_c("\\beta_{", 1:(dim(mat_vars)[1]),"}")) %>%
    add_column(par_sam=str_c("\\hat{\\beta}_{", 1:(dim(mat_vars)[1]),"}")) %>%
    mutate(sign_par="+")
  return(mat_vars) 
  
}

# function for casting the initial values

fun_lm_initial<-function(lm.mod, lm.dt, lm.val){
  table_val<-tibble(val_names=names(lm.val),   
                    val_give=format(lm.val,digits = 2))
  table_val<- table_val %>%
    separate(col=val_names,into=c("val_left", "val_right"),
             sep="_",fill="right",extra = "merge",remove =F) %>%
    mutate(val_latex=
             ifelse(is.na(val_right),
                    val_left, 
                    str_c(val_left,"_{",val_right,"}")))%>%
    mutate(val_line0=str_c(val_latex,"=",val_give,sep=""))  #obtain pattern "E(Y|age=35,sex_m=1)"
  return(table_val)
}


fun_lm_val<-function(lm.mod, lm.dt, lm.val){
  
  # get the model table and the give tabel 
  table_val<- fun_lm_initial(lm.mod, lm.dt, lm.val)  # the initial value table
  table_val<- dplyr::select(table_val,
                     val_names,val_give)                # dplyr::select needed column
  mat_vars<- fun_lm_vars(lm.mod, lm.dt) 
  mat_val<- dplyr::select(mat_vars,                        # the model table of variables
                   vars,left,right,par_pop,par_sam)
  mat_coef<- fun_lm_coef(lm.mod, lm.dt)
  mat_coef<- dplyr::select(mat_coef,
                    vars,coef,coef_sign,coef_d2)
  
  # check if the inital variabls is OK
  vars_left_fct<-factor(mat_val$left)
  vars_right_fct<-factor(mat_val$right)
  vars_fct<-fct_c(vars_left_fct,vars_right_fct)
  vars_levels<-levels(vars_fct)     
  val_levels<-levels(factor(table_val$val_names))
  check<-all(val_levels %in% vars_levels)          # make sure all "TRUE"
  
  if (!check){
    stop("initial name is not correct!\n please retry!")}
  else{
    mat_val<- left_join(mat_val,table_val,by=c("left"="val_names"))
    mat_val<- left_join(mat_val,table_val,by=c("right"="val_names"))
    
    mat_val<- mat_val %>%
      rename("val_left"="val_give.x",
             "val_right"="val_give.y") %>% # rename
      mutate(val_pop=ifelse(
        (is.na(val_left) & is.na(val_right)),
        "",
        ifelse(is.na(val_right),
               val_left,
               parse_double(val_left)*parse_double(val_right)))) %>% # parse character!
      type_convert(cols(val_pop=col_double())) %>% #convert column type to double
      mutate(val_pop_sign=ifelse(val_pop>0,"+",
                                 ifelse(val_pop<0,
                                        "-","")))  %>% #obtain the sign of the double
      mutate(val_pop_l1=ifelse(                                 # for line 1
        (is.na(val_left) & is.na(val_right)),
        par_pop,                                      # obtain pattern "b_0"
        ifelse(is.na(val_right),
               str_c(par_pop,"(",val_left,")"),       # obtain pattern "b_7(1)(-30)"
               str_c(par_pop,"(",val_left,")",
                     "\\cdot(",val_right,")") ))) %>% # obtain pattern "b_8(1)(-30)"
      mutate(val_pop_l2=ifelse(                                  # for line 2
        is.na(val_pop),
        str_c("+",par_pop),                           # obtain pattern "b_0"
        ifelse(val_pop==0,
               "",                                    # obtain pattern "b_1(0)"
               ifelse((val_pop==1)|(val_pop==-1),
                      str_c(val_pop_sign,par_pop),    # obtain pattern "b_2(-1)" or...
                      str_c(val_pop_sign,abs(val_pop),par_pop))))) %>% # use abs()!!
      add_column(val_align="&")    # add align
    
    mat_val<-bind_cols(mat_val,dplyr::select(mat_coef,coef,coef_sign,coef_d2))  # combine the coef table
    mat_val<- mat_val %>%
      mutate(val_sam=ifelse(is.na(val_pop),coef,          # compute the patter (coef_2)*(val_2), double
                            val_pop*coef)) %>%
      mutate(val_sam_line1=ifelse(is.na(val_pop), 
                                  par_sam,
                                  str_c(par_sam,"(",             # obtain pattern "b_2(35)"
                                        val_pop,")"))) %>%
      mutate(val_sam_line2=ifelse(is.na(val_pop),        
                                  str_c("[",
                                        formatC(val_sam,digits = 2,format="f"),"]"),   # obtain pattern "(-1.2)(35)"
                                  str_c("[",
                                        formatC(coef,digits = 2,format="f"),
                                        "]\\cdot(",val_pop,")")))
    return(mat_val)  
  }
}



fun_report_eq<-function(lm.mod,lm.dt,
                        lm.n=NULL,lm.label = NULL,
                        lm.simple = NULL){
  # call the function defined before
  mat_coef<-fun_lm_coef(lm.mod,lm.dt)
  table_lm<-fun_lm_output(lm.mod,lm.dt)
  mat_vars<-fun_lm_vars(lm.mod,lm.dt)
  k<-table_lm$k
  n<-table_lm$n
  # default number is k_mod
  if (is.null(lm.n)) {
    lm.n<-k
  }
  
  # functions of three lines for report equation
  fun_line_eq<-function(b,e){
    str_c(mat_vars$align_eq[b:e],mat_coef$coef_sign[b:e],mat_coef$coef_d2[b:e],mat_vars$vars_latex[b:e],collapse = '')
  }
  
  fun_line_t<-function(b,e){
    str_c(mat_vars$align_eq[b:e],'(',mat_coef$tstar_d4[b:e],')',collapse = '')
  }
  
  fun_line_se<-function(b,e){
    str_c(mat_vars$align_eq[b:e],'(',mat_coef$ste_d4[b:e],')',collapse = '')
  }
  
  # split lines if equation is too long(more than 5 X)
  
  if (k<=lm.n) {
    cat(
      "$$\\begin{equation}",
      paste0('\\begin{alignedat}{',999,'}'),
      paste0('&\\widehat{',table_lm$name.y,'}=',fun_line_eq(1,k),'\\\\'),
      if (!isTRUE(lm.simple)){
        str_c(
        paste0('&\\text{(t)}',fun_line_t(1,k),'\\\\'),
        paste0('&\\text{(se)}',fun_line_se(1,k),'\\\\'),
        paste0('&\\text{(fitness)}',
               #'&& n=',table_lm$n,";",
               '&& R^2=',formatC(table_lm$R2, digits = 4,format = "f"),";",
               '&& \\bar{R^2}=',formatC(table_lm$R2.adj, digits = 4,format = "f"),
               "\\\\"),
        paste0('& ',
               '&& F^{\\ast}=',formatC(table_lm$F.star, digits = 2,format = "f"),";",
               '&& p=',formatC(table_lm$F.p, digits=4,format = "f")))
      },
      "\\end{alignedat}",
      # default no equation label
      if (!is.null(lm.label)) {
        (paste0('(\\#eq:',lm.label,')'))
      },
      "\\end{equation}$$"
      )
  } else if ((k>lm.n)&(k<=2*lm.n)){
    cat(
      "$$\\begin{equation}",
      paste0('\\begin{alignedat}{',999,'}'),
      
      paste0('&\\widehat{',table_lm$name.y,'}=',fun_line_eq(1,lm.n),'\\\\'),
      
      if (!isTRUE(lm.simple)){
        str_c(
          paste0('&\\text{(t)}',fun_line_t(1,lm.n),'\\\\'),
          paste0('&\\text{(se)}',fun_line_se(1,lm.n),'\\\\'),
          
          paste0('&\\text{(cont.)}',fun_line_eq(lm.n+1,k),'\\\\'),
          paste0('&\\text{(t)}',fun_line_t(lm.n+1,k),'\\\\'),
          paste0('&\\text{(se)}',fun_line_se(lm.n+1,k),'\\\\'),
          
          paste0('&\\text{(fitness)}',
                 #'&& n=',table_lm$n,";",
                 '&& R^2=',formatC(table_lm$R2, digits = 4,format = "f"),";",
                 '&& \\bar{R^2}=',formatC(table_lm$R2.adj, digits = 4,format = "f"),
                 "\\\\"),
          paste0('& ',
                 '&& F^{\\ast}=',formatC(table_lm$F.star, digits = 2,format = "f"),";",
                 '&& p=',formatC(table_lm$F.p, digits=4,format = "f"),
                 "\\\\"))
      },
      "\\end{alignedat}",
      # default no equation label
      if (!is.null(lm.label)) {
        (paste0('(\\#eq:',lm.label,')'))
      },
      "\\end{equation}$$"
      )
  } else if (((k>2*lm.n)&(k<=3*lm.n))){
    cat(
      "$$\\begin{equation}",
      paste0('\\begin{alignedat}{',999,'}'),
      paste0('&\\widehat{',table_lm$name.y,'}=',fun_line_eq(1,lm.n),'\\\\'),
      
      if (!isTRUE(lm.simple)){
        str_c(
          paste0('&\\text{(t)}',fun_line_t(1,lm.n),'\\\\'),
          paste0('&\\text{(se)}',fun_line_se(1,lm.n),'\\\\'),
          
          paste0('&\\text{(cont.)}',fun_line_eq(lm.n+1,2*lm.n),'\\\\'),
          paste0('&\\text{(t)}',fun_line_t(lm.n+1,2*lm.n),'\\\\'),
          paste0('&\\text{(se)}',fun_line_se(lm.n+1,2*lm.n),'\\\\'),
          
          paste0('&\\text{(cont.)}',fun_line_eq(2*lm.n+1,k),'\\\\'),
          paste0('&\\text{(t)}',fun_line_t(2*lm.n+1,k),'\\\\'),
          paste0('&\\text{(se)}',fun_line_se(2*lm.n+1,k),'\\\\'),
          
          paste0('&\\text{(fitness)}',
                 #'&& n=',table_lm$n,";",
                 '&& R^2=',formatC(table_lm$R2, digits = 4,format = "f"),";",
                 '&& \\bar{R^2}=',formatC(table_lm$R2.adj, digits = 4,format = "f"),
                 "\\\\"),
          paste0('& ',
                 '&& F^{\\ast}=',formatC(table_lm$F.star, digits = 2,format = "f"),";",
                 '&& p=',formatC(table_lm$F.p, digits=4,format = "f"),
                 "\\\\"))
        },
      "\\end{alignedat}",
      # default no equation label
      if (!is.null(lm.label)) {
        (paste0('(\\#eq:',lm.label,')'))
      },
      "\\end{equation}$$"
      )
  }
  else print("can not show equation more than three rows !")  
  
}


fun_report_prm<-function(lm.mod,lm.dt,
                         lm.n=NULL,lm.label = NULL, lm.t=FALSE){
  # call the function defined before
  mat_coef<-fun_lm_coef(lm.mod,lm.dt)
  table_lm<-fun_lm_output(lm.mod,lm.dt)
  mat_vars<-fun_lm_vars(lm.mod,lm.dt)
  k<-table_lm$k
  n<-table_lm$n
  # default number is k_mod
  if (is.null(lm.n)) {
    lm.n<-k
  }
  
  # function for math equation line
  fun_line_math<-function(b,e){
    paste(mat_vars$align_eq[b:e],
          mat_vars$sign_par[b:e],
          mat_vars$par_pop[b:e],
          mat_vars$vars_latex[b:e],
          collapse = "")
  }
  # split lines if equation is too long(more than 5 X)
  if (k<=lm.n) {
    cat("$$\\begin{equation}",
        str_c('\\begin{alignedat}{',999,"}"),
        paste0("&",table_lm$name.y,'=',fun_line_math(1,k),
               if (lm.t){'&&+u_t\\\\'} else {'&&+u_i\\\\'}),
        "\\end{alignedat}",
        # default no equation label
        if (!is.null(lm.label)) {
          paste0('(\\#eq:',lm.label,')')},
        "\\end{equation}$$"
        )
  } else if ((k > lm.n)&(k <= 2*lm.n)){
    cat("$$\\begin{equation}",
        str_c('\\begin{alignedat}{',999,"}"),
        paste0("&",table_lm$name.y,'=',fun_line_math(1,lm.n),'\\\\'),
        paste0('&\\text{(cont.)}',fun_line_math(lm.n+1,k),
               if (lm.t){'&&+u_t\\\\'} else {'&&+u_i\\\\'}),
        "\\end{alignedat}",
        # default no equation label
        if (!is.null(lm.label)) {
          paste0('(\\#eq:',lm.label,')')},
        "\\end{equation}$$"
        )
  } else if ((k > 2*lm.n)&(k <= 3*lm.n)){
    cat("$$\\begin{equation}",
        str_c('\\begin{alignedat}{',999,"}"),
        paste0("&",table_lm$name.y,'=',fun_line_math(1,lm.n),'\\\\'),
        paste0('&\\text{(cont.)}',fun_line_math(lm.n+1,2*lm.n),'\\\\'),
        paste0('&\\text{(cont.)}',fun_line_math(2*lm.n+1,k),
               if (lm.t){'&&+u_t\\\\'} else {'&&+u_i\\\\'}),
        "\\end{alignedat}",
        # default no equation label
        if (!is.null(lm.label)) {
          paste0('(\\#eq:',lm.label,')')},
        "\\end{equation}$$"
        )
  }
  else {
    stop("can not show equation more than three rows !")}
}

fun_report_srm<-function(lm.mod,lm.dt,
                         lm.n=NULL,lm.label = NULL,lm.t=FALSE){
  # call the function defined before
  mat_coef<-fun_lm_coef(lm.mod,lm.dt)
  table_lm<-fun_lm_output(lm.mod,lm.dt)
  mat_vars<-fun_lm_vars(lm.mod,lm.dt)
  k<-table_lm$k
  n<-table_lm$n
  # default number is k_mod
  if (is.null(lm.n)) {
    lm.n<-k
  }
  
  # function for math equation line
  fun_line_math<-function(b,e){
    paste(mat_vars$align_eq[b:e],
          mat_vars$sign_par[b:e],
          mat_vars$par_sam[b:e],
          mat_vars$vars_latex[b:e],
          collapse = "")
  }
  # split lines if equation is too long(more than 5 X)
  if (k<=lm.n) {
    cat("$$\\begin{equation}",
        str_c('\\begin{alignedat}{',999,"}"),
        paste0("&",table_lm$name.y,'=',fun_line_math(1,k),
               if (lm.t){'&&+e_t\\\\'} else {'&&+e_i\\\\'}),
        "\\end{alignedat}",
        # default no equation label
        if (!is.null(lm.label)) {
          paste0('(\\#eq:',lm.label,')')},
        "\\end{equation}$$"
    )
  } else if ((k > lm.n)&(k <= 2*lm.n)){
    cat("$$\\begin{equation}",
        str_c('\\begin{alignedat}{',999,"}"),
        paste0("&",table_lm$name.y,'=',fun_line_math(1,lm.n),'\\\\'),
        paste0('&\\text{(cont.)}',fun_line_math(lm.n+1,k),
               if (lm.t){'&&+e_t\\\\'} else {'&&+e_i\\\\'}),
        "\\end{alignedat}",
        # default no equation label
        if (!is.null(lm.label)) {
          paste0('(\\#eq:',lm.label,')')},
        "\\end{equation}$$"
    )
  } else if ((k > 2*lm.n)&(k <= 3*lm.n)){
    cat("$$\\begin{equation}",
        str_c('\\begin{alignedat}{',999,"}"),
        paste0("&",table_lm$name.y,'=',fun_line_math(1,lm.n),'\\\\'),
        paste0('&\\text{(cont.)}',fun_line_math(lm.n+1,2*lm.n),'\\\\'),
        paste0('&\\text{(cont.)}',fun_line_math(2*lm.n+1,k),
               if (lm.t){'&&+e_t\\\\'} else {'&&+e_i\\\\'}),
        "\\end{alignedat}",
        # default no equation label
        if (!is.null(lm.label)) {
          paste0('(\\#eq:',lm.label,')')},
        "\\end{equation}$$"
    )
  }
  else {
    stop("can not show equation more than three rows !")}
}


fun_report_srf<-function(lm.mod,lm.dt,
                         lm.n=NULL,lm.label = NULL){
  # call the function defined before
  mat_coef<-fun_lm_coef(lm.mod,lm.dt)
  table_lm<-fun_lm_output(lm.mod,lm.dt)
  mat_vars<-fun_lm_vars(lm.mod,lm.dt)
  k<-table_lm$k
  n<-table_lm$n
  # default number is k_mod
  if (is.null(lm.n)) {
    lm.n<-k
  }
  
  # function for math equation line
  fun_line_math<-function(b,e){
    paste(mat_vars$align_eq[b:e],
          mat_vars$sign_par[b:e],
          mat_vars$par_sam[b:e],
          mat_vars$vars_latex[b:e],
          sep='',collapse = '')
  }
  # split lines if equation is too long(more than 5 X)
  if (k<=lm.n) {
    cat("$$\\begin{equation}",
        str_c('\\begin{alignedat}{',999,"}"),
        paste0("&\\widehat{",table_lm$name.y,'}=',fun_line_math(1,k),'\\\\'),
        "\\end{alignedat}",
        # default no equation label
        if (!is.null(lm.label)) {
          paste0('(\\#eq:',lm.label,')')},
        "\\end{equation}$$"
        )
  } else if ((k > lm.n)&(k <= 2*lm.n)){
    cat("$$\\begin{equation}",
        str_c('\\begin{alignedat}{',999,"}"),
        paste0("&\\widehat{",table_lm$name.y,'}=',fun_line_math(1,lm.n),'\\\\'),
        paste0('&\\text{(cont.)}',fun_line_math(lm.n+1,k),'\\\\'),
        "\\end{alignedat}",
        # default no equation label
        if (!is.null(lm.label)) {
          paste0('(\\#eq:',lm.label,')')},
        "\\end{equation}$$"
        )
  } else if ((k > 2*lm.n)&(k <= 3*lm.n)){
    cat("$$\\begin{equation}",
        str_c('\\begin{alignedat}{',999,"}"),
        paste0("&\\widehat{",table_lm$name.y,'}=',fun_line_math(1,lm.n),'\\\\'),
        paste0('&\\text{(cont.)}',fun_line_math(lm.n+1,2*lm.n),'\\\\'),
        paste0('&\\text{(cont.)}',fun_line_math(2*lm.n+1,k),'\\\\'),
        "\\end{alignedat}",
        # default no equation label
        if (!is.null(lm.label)) {
          paste0('(\\#eq:',lm.label,')')},
        "\\end{equation}$$"
        )
  }
  else {
    stop("can not show equation more than three rows !")}
}


fun_report_prm_val<-function(lm.mod,lm.dt,
                             lm.n=NULL,lm.label = NULL,
                             lm.val){
  # call the function defined before
  table_lm<-fun_lm_output(lm.mod,lm.dt)
  table_val<-fun_lm_initial(lm.mod,lm.dt,lm.val)
  mat_val<-fun_lm_val(lm.mod,lm.dt,lm.val)
  
  k<-table_lm$k
  n<-table_lm$n
  # default number is k_mod
  if (is.null(lm.n)) {
    lm.n<-k
  }
  
  # split lines if equation is too long(more than 5 X)
  if (k<=lm.n) {
    cat(
      '$$\\begin{align}',
      #'\\begin{split}',
      str_c('E(',table_lm$name.y,'&|',
                   str_c(table_val$val_line0, collapse = ";"), 
                   ")\\\\"), 
      str_c("=&",
            str_c("+",mat_val$val_pop_l1[1:k],collapse = ""),'\\\\'),
      str_c("=&",
            str_c(mat_val$val_pop_l2[1:k],collapse = ""),"\\\\"),
      #'\\end{split}', 
      # default no equation label
       if (!is.null(lm.label)) {
         paste0('(\\#eq:',lm.label,')')
       }, 
      '\\end{align}$$',
      sep = '\n')
  } else if ((k > lm.n)&(k <= 2*lm.n)){
    cat(
      '$$\\begin{align}',
      #'\\begin{split}',
      str_c('E(',table_lm$name.y,'&|',
            str_c(table_val$val_line0, collapse = ";"), ")\\\\"),
      str_c("=&",
            str_c("+",mat_val$val_pop_l1[1:lm.n],collapse = ""),'\\\\'),
      str_c("&",
            str_c("+",mat_val$val_pop_l1[(lm.n+1):k],collapse = ""),'\\\\'),
      str_c("=&",
            str_c(mat_val$val_pop_l2[1:lm.n],collapse = ""),"\\\\"),
      str_c("&",
            str_c(mat_val$val_pop_l2[(lm.n+1):k],collapse = ""),"\\\\"),
      #'\\end{split}',
       # default no equation label
       if (!is.null(lm.label)) {
         (paste0('(\\#eq:',lm.label,')'))
       },
      '\\end{align}$$',
      sep = '\n')
  } else if ((k > 2*lm.n)&(k <= 3*lm.n)){
    cat(
      '$$\\begin{align}',
      #'\\begin{split}',
      str_c('E(',table_lm$name.y,'&|',
            str_c(table_val$val_line0, collapse = ";"), ")\\\\"), 
      str_c("=&",
            str_c("+",mat_val$val_pop_l1[1:lm.n],collapse = ""),'\\\\'),
      str_c("&",
            str_c("+",mat_val$val_pop_l1[(lm.n+1):2*lm.n],collapse = ""),'\\\\'),
      str_c("&",
            str_c("+",mat_val$val_pop_l1[(2*lm.n+1):k],collapse = ""),'\\\\'),
      str_c("=&",
            str_c(mat_val$val_pop_l2[1:2*lm.n],collapse = ""),"\\\\"),
      str_c("&",
            str_c(mat_val$val_pop_l2[(lm.n+1):2*lm.n],collapse = ""),"\\\\"),
      str_c("&",
            str_c(mat_val$val_pop_l2[(2*lm.n+1):k],collapse = ""),"\\\\"),
      #'\\end{split}',
      # default no equation label
      if (!is.null(lm.label)) {
        (paste0('(\\#eq:',lm.label,')'))
      },
      '\\end{align}$$',
      sep = '\n')
  }  else if((k > 3*lm.n)) {
    stop("can not show more than three rows!\n please retry !")
  }
}



fun_report_srf_val<-function(lm.mod,lm.dt,lm.val,
                             lm.n=NULL,lm.label =NULL){
  # call the function defined before
  table_lm<-fun_lm_output(lm.mod,lm.dt)
  mat_val<-fun_lm_val(lm.mod,lm.dt,lm.val)
  table_val<-fun_lm_initial(lm.mod,lm.dt,lm.val)
  k<-table_lm$k
  n<-table_lm$n
  # default number is k_mod
  if (is.null(lm.n)) {
    lm.n<-k
  }
  
  # split lines if equation is too long(more than 5 X)
  if (k<=lm.n) {
    cat(
      '$$\\begin{align}',
      #'\\begin{split}',
      str_c('(\\widehat{',table_lm$name.y,'}&|(',
            str_c(table_val$val_line0, collapse = ";") ,")\\\\"),
      str_c("=&",str_c("+",mat_val$val_sam_line1[1:k],collapse = ""),'\\\\'),
      str_c("=&",str_c("+",mat_val$val_sam_line2[1:k],collapse = ""),"\\\\"),
      str_c("=&",formatC(sum(mat_val$val_sam), digits = 4, format="f"),"\\\\"),
      #'\\end{split}',
      # default no equation label
      if (!is.null(lm.label)) {
        (paste0('(\\#eq:',lm.label,')'))
      },
      '\\end{align}$$',
      sep = '\n')
  } else if ((k > lm.n)&(k <= 2*lm.n)){
    cat(
      '$$\\begin{align}',
      #'\\begin{split}',
      str_c('(\\widehat{',table_lm$name.y,'}&|',str_c(table_val$val_line0, collapse = ";"), ")\\\\"),
      str_c("=&",str_c("+",mat_val$val_sam_line1[1:lm.n],collapse = ""),'\\\\'),
      str_c("&",str_c("+",mat_val$val_sam_line1[(lm.n+1):k],collapse = ""),'\\\\'),
      str_c("=&",str_c("+",mat_val$val_sam_line2[1:lm.n],collapse = ""),"\\\\"),
      str_c("&",str_c("+",mat_val$val_sam_line2[(lm.n+1):k],collapse = ""),"\\\\"),
      str_c("=&",formatC(sum(mat_val$val_sam), digits = 4, format="f"),"\\\\"),
      #'\\end{split}',
      # default no equation label
      if (!is.null(lm.label)) {
        (paste0('(\\#eq:',lm.label,')'))
      },
      '\\end{align}$$',
      sep = '\n')
  } else if ((k > 2*lm.n)&(k <= 3*lm.n)){
    cat(
      '$$\\begin{align}',
      #'\\begin{split}',
      str_c('(\\widehat{',table_lm$name.y,'}&|',str_c(table_val$val_line0, collapse = ";"), ")\\\\"), 
      str_c("=&",str_c("+",mat_val$val_sam_line1[1:lm.n],collapse = ""),'\\\\'),
      str_c("&",str_c("+",mat_val$val_sam_line1[(lm.n+1):2*lm.n],collapse = ""),'\\\\'),
      str_c("&",str_c("+",mat_val$val_sam_line1[(2*lm.n+1):k],collapse = ""),'\\\\'),
      str_c("=&",str_c("+",mat_val$val_sam_line2[1:2*lm.n],collapse = ""),"\\\\"),
      str_c("&",str_c("+",mat_val$val_sam_line2[(lm.n+1):2*lm.n],collapse = ""),"\\\\"),
      str_c("&",str_c("+",mat_val$val_sam_line2[(2*lm.n+1):k],collapse = ""),"\\\\"),
      str_c("=&",formatC(sum(mat_val$val_sam), digits = 4, format="f"),"\\\\"),
      #'\\end{split}',
      # default no equation label
      if (!is.null(lm.label)) {
        (paste0('(\\#eq:',lm.label,')'))
      },
      '\\end{align}$$',
      sep = '\n')
  }  else if((k > 3*lm.n)) {
    stop("can not show more than three rows!\n please retry !")
  }
}
