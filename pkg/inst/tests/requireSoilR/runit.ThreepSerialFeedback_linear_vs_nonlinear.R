#
# vim:set ff=unix expandtab ts=2 sw=2:
# 	|C_am 	|	|X_01| 
# 	|C_as	|	|X_02|
# 	|C_bm	|	|X_03|
# 	|C_bs	|	|X_04|
# 	|C_fw	|	|X_05|
# 	|C_acw	|	|X_06|
# 	|C_bcw	|	|X_07|
# 	|C_mic	|	|X_08|
# 	|C_slo	|	|X_09|
# X =	|C_pas	|   =   |X_10| 
# 	|N_am	|	|X_11|
# 	|N_as	|	|X_12|
# 	|N_bm	|	|X_13|
# 	|N_bs	|	|X_14|
# 	|N_fw	|	|X_15|
# 	|N_acw	|	|X_16|
# 	|N_bcw	|	|X_17|
# 	|N_mic	|	|X_18|
# 	|N_slo	|	|X_19|
# 	|N_pas	|	|X_20|
# 	|N_ino	|	|X_21|

test.corrado=function(){
  require(RUnit)
  chi<-function(t){
	# here goes the real stuff 
	# there are also some moisture,Temp dependent function
	# already in SoilR
  	t
  } 
  phi_mn<-function(X){
	# here goes the real stuff 
	# there are also some moisture,Temp dependent function
	# already in SoilR
  	3
  } 
  kam<-0.3
  #...
  t_start=0
  t_end=20
  nr=11
  tn=100
  tol=.02/tn
  
  internal_fluxes=list()
  # flux from C_am to C_mic
  internal_fluxes[["1_to_8"]]=function(X,t){
      # 0.4 * kam * chi(t) * C_am(t) * phi_mn(X)
      C_am<-X[1]
      0.4 * kam * chi(t) * C_am(t) * phi_mn(X)
  }
  internal_fluxes[["3_to_2"]]=function(C,t){2}
}
