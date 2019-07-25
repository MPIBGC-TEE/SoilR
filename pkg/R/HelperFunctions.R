fromToSplitter=function(){"_to_"}
rightArrowSplitter=function(){"->"}

src_to_dest_parts<-function(src_to_dest){
  res_Arr= unlist(strsplit(src_to_dest,split=rightArrowSplitter()))
  if (length(res_Arr)==2){
     return(res_Arr)
  }else{
    warning("The format for strings describing pool connections has changed from 
     '1_to_2' to '1->2' which is safer and also works with variable names
     like 'a->b' . Please change your code accordingly")
    res_old= unlist(strsplit(src_to_dest,split=fromToSplitter()))
    if (length(res_old)==2){
        return(res_old)
    } else {
        stop("could not split: The string has to have the form '1->2' or 'a->b' where 'a' and 'b'
             are pool names.")
    }
  }
}
getRecipient=function(src_to_dest){
  #as.numeric(unlist(strsplit(stri,split=fromToSplitter()))[[2]])
  as.numeric(src_to_dest_parts(src_to_dest)[[2]])
}
getSender=function(src_to_dest){
  #as.numeric(unlist(strsplit(stri,split=fromToSplitter()))[[1]])
  as.numeric(src_to_dest_parts(src_to_dest)[[1]])
}
key=function(i,j){
  paste(i,fromToSplitter(),j,sep="")
}
stockKey=function(ip){paste("particles_in_pool_",ip,sep="")}
leaveKey=function(ip){paste("particles_leaving_pool_",ip,sep="")}
