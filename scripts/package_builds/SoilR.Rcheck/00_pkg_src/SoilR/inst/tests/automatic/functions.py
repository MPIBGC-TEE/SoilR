def rmatrixprint(m,shift):
    r=m.rows
    c=m.cols
    sh1=shift+"  "
    sh2=sh1+"   "
    s="matrix(\n"\
    +sh1+"nrow="+str(r)+",\n"\
    +sh1+"ncol="+str(c)+",\n"\
    +sh1+"c(\n"
    for j in range(c): #note that the ordering of the indices is different in R which fills columns first
        s+=sh2
        for i in range(r):
            suff=",  "
            s+=(str(m[i,j])+suff)
        s+="\n"
    s=s[:-(len(suff)+1)]#remove last ",  \n"
    s+="\n"+sh1+")\n"+shift+")"
    return s
def rlistprint(v,shift):
    print(v)
    l=len(v)
    sh1=shift+" "
    sh2=sh1+"   "
    s=sh1+"c(\n"
    suff=",\n"
    for j in range(l-1): 
        s+=(sh2+str(v[j])+suff)
    s+=(sh2+str(v[l-1])+"\n"+sh1+")")    
    return s
