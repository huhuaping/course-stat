simple.eda.ts             package:Simple             R Documentation

_M_a_k_e_s _3 _u_s_e_f_u_l _g_r_a_p_h_s _f_o_r _e_d_a _o_f _t_i_m_e_s _s_e_r_i_e_s

_D_e_s_c_r_i_p_t_i_o_n:

     This makes 3 graphs to check for serial correlation in data. The
     graphs are a sequential plot (i vs X_i), a lag plot (plotting X_i
     vs X_{i+k} where k=1 by default) and an autocorrelation plot from
     the times series ("ts") package.

_U_s_a_g_e:

     simple.eda.ts(x, lag=1)

_A_r_g_u_m_e_n_t_s:

       x: a univariate vector of data 

     lag: a lag to give to the lag plot 

_V_a_l_u_e:

     Makes the graph with 1 row, 3 columns

_A_u_t_h_o_r(_s):

     John Verzani

_R_e_f_e_r_e_n_c_e_s:

     see
     \href{http://www.itl.nist.gov/div898/handbook/eda/section3/eda34.h
     tm} for more information on these and other plots for exploratory
     data analysis.

_E_x_a_m_p_l_e_s:

     ## The function is currently defined as

     ## look for no correlation
     x <- rnorm(100);simple.eda.ts(x)
     ## you will find correlation here
     simple.eda.ts(cumsum(x))

