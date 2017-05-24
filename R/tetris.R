library(raster)
library(grid)
tetris<-function(length,width,data=rep(0,length*width)){
  data=matrix(0,nrow=length,ncol=width)
  update.plot<-function(length,width,curvec){
    rplot<-raster(xmn=0,xmx=width,ymn=0,ymx=length,nrows=length,ncols=width)
    rplot[]<-rep(0,length*width)
    rcolors<-data
    plot(rplot,axes=FALSE, box=FALSE,legend=F,main="Tetris in R!",col=rcolors)
    plot(rasterToPolygons(rplot), add=TRUE, border='black', lwd=1,col=rcolors) 
  }
  
  shapes<-function(shape,rotation){
    L1shape<-function(pos){
      if(pos==1){
        return(matrix(c(0,0,0,0,0,
                        0,0,1,0,0,
                        0,0,1,0,0,
                        0,0,1,1,0,
                        0,0,0,0,0),nrow=5,byrow = T))
      } else if(pos==2){
        return(matrix(c(0,0,0,0,0,
                        0,0,0,0,0,
                        0,1,1,1,0,
                        0,1,0,0,0,
                        0,0,0,0,0),nrow=5,byrow = T))
      } else if(pos==3){
        return(matrix(c(0,0,0,0,0,
                        0,1,1,0,0,
                        0,0,1,0,0,
                        0,0,1,0,0,
                        0,0,0,0,0),nrow=5,byrow = T))
      } else if(pos==4){
        return(matrix(c(0,0,0,0,0,
                        0,0,0,1,0,
                        0,1,1,1,0,
                        0,0,0,0,0,
                        0,0,0,0,0),nrow=5,byrow = T))
      } else return("sorry something is wrong")
    }
    L2shape<-function(pos){
      if(pos==1){
        return(matrix(c(0,0,0,0,0,
                        0,0,1,0,0,
                        0,0,1,0,0,
                        0,1,1,0,0,
                        0,0,0,0,0),nrow=5,byrow = T))
      } else if(pos==2){
        return(matrix(c(0,0,0,0,0,
                        0,0,0,0,0,
                        0,1,1,1,0,
                        0,0,0,1,0,
                        0,0,0,0,0),nrow=5,byrow = T))
      } else if(pos==3){
        return(matrix(c(0,0,0,0,0,
                        0,0,1,1,0,
                        0,0,1,0,0,
                        0,0,1,0,0,
                        0,0,0,0,0),nrow=5,byrow = T))
      } else if(pos==4){
        return(matrix(c(0,0,0,0,0,
                        0,0,0,0,0,
                        0,1,1,1,0,
                        0,0,0,1,0,
                        0,0,0,0,0),nrow=5,byrow = T))
      } else return("sorry something is wrong")
    }
    lineshape<-function(pos){
      if(pos==1){
        return(matrix(c(0,0,1,0,0,
                        0,0,1,0,0,
                        0,0,1,0,0,
                        0,0,1,0,0,
                        0,0,0,0,0),nrow=5,byrow = T))
      } else if(pos==2){
        return(matrix(c(0,0,0,0,0,
                        0,0,0,0,0,
                        1,1,1,1,0,
                        0,0,0,0,0,
                        0,0,0,0,0),nrow=5,byrow = T))
      } else if(pos==3){
        return(matrix(c(0,0,0,0,0,
                        0,0,1,0,0,
                        0,0,1,0,0,
                        0,0,1,0,0,
                        0,0,1,0,0),nrow=5,byrow = T))
      } else if(pos==4){
        return(matrix(c(0,0,0,0,0,
                        0,0,0,0,0,
                        0,1,1,1,1,
                        0,0,0,0,0,
                        0,0,0,0,0),nrow=5,byrow = T))
      } else return("sorry something is wrong")
    }
    squareshape<-function(pos){
      if(pos==1){
        return(matrix(c(0,0,0,0,0,
                        0,0,0,0,0,
                        0,0,1,1,0,
                        0,0,1,1,0,
                        0,0,0,0,0),nrow=5,byrow = T))
      } else if(pos==2){
        return(matrix(c(0,0,0,0,0,
                        0,0,0,0,0,
                        0,0,1,1,0,
                        0,0,1,1,0,
                        0,0,0,0,0),nrow=5,byrow = T))
      } else if(pos==3){
        return(matrix(c(0,0,0,0,0,
                        0,0,0,0,0,
                        0,0,1,1,0,
                        0,0,1,1,0,
                        0,0,0,0,0),nrow=5,byrow = T))
      } else if(pos==4){
        return(matrix(c(0,0,0,0,0,
                        0,0,0,0,0,
                        0,0,1,1,0,
                        0,0,1,1,0,
                        0,0,0,0,0),nrow=5,byrow = T))
      } else return("sorry something is wrong")
    }
    N1shape<-function(pos){
      if(pos==1){
        return(matrix(c(0,0,0,0,0,
                        0,0,0,1,0,
                        0,0,1,1,0,
                        0,0,1,0,0,
                        0,0,0,0,0),nrow=5,byrow = T))
      } else if(pos==2){
        return(matrix(c(0,0,0,0,0,
                        0,0,0,0,0,
                        0,1,1,0,0,
                        0,0,1,1,0,
                        0,0,0,0,0),nrow=5,byrow = T))
      } else if(pos==3){
        return(matrix(c(0,0,0,0,0,
                        0,0,1,0,0,
                        0,1,1,0,0,
                        0,1,0,0,0,
                        0,0,0,0,0),nrow=5,byrow = T))
      }  else if(pos==4){
        return(matrix(c(0,0,0,0,0,
                        0,1,1,0,0,
                        0,0,1,1,0,
                        0,0,0,0,0,
                        0,0,0,0,0),nrow=5,byrow = T))
      } else return("sorry something is wrong")
    }
    N2shape<-function(pos){
      if(pos==1){
        return(matrix(c(0,0,0,0,0,
                        0,0,1,0,0,
                        0,0,1,1,0,
                        0,0,0,1,0,
                        0,0,0,0,0),nrow=5,byrow = T))
      } else if(pos==2){
        return(matrix(c(0,0,0,0,0,
                        0,0,0,0,0,
                        0,0,1,1,0,
                        0,1,1,0,0,
                        0,0,0,0,0),nrow=5,byrow = T))
      } else if(pos==3){
        return(matrix(c(0,0,0,0,0,
                        0,1,0,0,0,
                        0,1,1,0,0,
                        0,0,1,0,0,
                        0,0,0,0,0),nrow=5,byrow = T))
      }  else if(pos==4){
        return(matrix(c(0,0,0,0,0,
                        0,0,1,1,0,
                        0,1,1,0,0,
                        0,0,0,0,0,
                        0,0,0,0,0),nrow=5,byrow = T))
      } else return("sorry something is wrong")
    }
    Tshape<-function(pos){
      if(pos==1){
        return(matrix(c(0,0,0,0,0,
                        0,0,1,0,0,
                        0,0,1,1,0,
                        0,0,1,0,0,
                        0,0,0,0,0),nrow=5,byrow = T))
      } else if(pos==2){
        return(matrix(c(0,0,0,0,0,
                        0,0,0,0,0,
                        0,1,1,1,0,
                        0,0,1,0,0,
                        0,0,0,0,0),nrow=5,byrow = T))
      } else if(pos==3){
        return(matrix(c(0,0,0,0,0,
                        0,0,1,0,0,
                        0,1,1,0,0,
                        0,0,1,0,0,
                        0,0,0,0,0),nrow=5,byrow = T))
      }  else if(pos==4){
        return(matrix(c(0,0,0,0,0,
                        0,0,1,0,0,
                        0,1,1,1,0,
                        0,0,0,0,0,
                        0,0,0,0,0),nrow=5,byrow = T))
      } else return("sorry something is wrong")
    }
    trim<-function(shape){
      return(shape[!rowSums(shape)==0,!colSums(shape)==0])
    }
    if(shape==1){
      return(trim(L1shape(rotation)))
    } else if(shape==2){
      return(trim(L2shape(rotation)))
    } else if(shape==3){
      return(trim(lineshape(rotation)))
    } else if(shape==4){
      return(trim(squareshape(rotation)))
    } else if(shape==5){
      return(trim(N1shape(rotation)))
    } else if(shape==6){
      return(trim(N2shape(rotation)))
    } else if(shape==7){
      return(trim(Tshape(rotation)))
    } else "sorry wrong shape"
  }
  
  startshape<-function(){
    shp<-shapes(sample(1:7,1),sample(1:4,1))
    xrand<-sample(1:width,1)
    cnt<-0
    
     while((!length(xrand:width)>=ncol(shp)) & cnt<100){
       xrand<-sample(1:width,1);cnt=cnt+1
     }
    return(list(startpos=xrand,shape=shp))
  }
  
  sts<-startshape()
  while(!any(data[1:dim(sts$shape)[1],sts$startpos:(sts$startpos+dim(sts$shape)[2]-1)]==1)){
    
  }
    
  
  
  
}
