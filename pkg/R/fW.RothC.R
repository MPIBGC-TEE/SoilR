#' Effects of moisture on decomposition rates according to the RothC model
#' 
#' Calculates the effects of moisture (precipitation and pan evaporation) on
#' decomposition rates according to the RothC model.
#' 
#' 
#' @param P A vector with monthly precipitation (mm).
#' @param E A vector with same length with open pan evaporation or
#' evapotranspiration (mm).
#' @param S.Thick Soil thickness in cm. Default for Rothamsted is 23 cm.
#' @param pClay Percent clay.
#' @param pE Evaporation coefficient. If open pan evaporation is used pE=0.75.
#' If Potential evaporation is used, pE=1.0.
#' @param bare Logical. Under bare soil conditions, bare=TRUE. Dafault is set
#' under vegetated soil.
#' @return A data.frame with accumulated top soil moisture deficit (Acc.TSMD)
#' and the rate modifying factor b.
#' @references Coleman, K., and D. S. Jenkinson (1999), RothC-26.3 A model for
#' the turnover of carbon in soil: model description and windows user guide
#' (modified 2008), 47 pp, IACR Rothamsted, Harpenden.
fW.RothC<- structure(
    function 
    (P,           
     E,           
     S.Thick=23,  
     pClay=23.4,  
     pE=0.75,     
     bare=FALSE   
     )
    {  
     B=ifelse(bare == FALSE,1,1.8)
     Max.TSMD=-(20+1.3*pClay-0.01*(pClay^2))*(S.Thick/23)*(1/B)
     M=P-E*pE
     Acc.TSMD=NULL
       for(i in 2:length(M)){
          Acc.TSMD[1]=ifelse(M[1] > 0, 0, M[1])
          if(Acc.TSMD[i-1]+M[i] < 0){
             Acc.TSMD[i]=Acc.TSMD[i-1]+M[i]
          }
            else(Acc.TSMD[i]=0)
         if(Acc.TSMD[i]<=Max.TSMD) {
            Acc.TSMD[i]=Max.TSMD
         }
       }
     b=ifelse(Acc.TSMD > 0.444*Max.TSMD,1,(0.2+0.8*((Max.TSMD-Acc.TSMD)/(Max.TSMD-0.444*Max.TSMD))))
     return(data.frame(Acc.TSMD,b))
     }
     ,
    ex=function(){
       P=c(74,59,62,51,52,57,34,55,58,56,75,71) 
       E=c(8,10,27,49,83,99,103,91,69,34,16,8)  
       Rothamsted=fW.RothC(P,E)
       data.frame(month.name,P,E,0.75*E,P-0.75*E,Rothamsted)  
    }        
)
