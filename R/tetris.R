library(raster)
library(grid)
tetris<-function(length,width,data=rep(0,length*width)){
  data=matrix('white',nrow=length,ncol=width)
  
  update.plot<-function(length,width,data){
    rplot<-raster(xmn=0,xmx=width,ymn=0,ymx=length,nrows=length,ncols=width)
    rplot[]<-rep(0,length*width)
    rcolors<-data
    plot(rplot,axes=FALSE, box=FALSE,legend=F,main="Tetris in R!",col=t(rcolors))
    plot(rasterToPolygons(rplot), add=TRUE, border='black', lwd=2,col=t(rcolors))
  }
  
  cols<-c("deeppink1","grey","red","blue","cyan4","green","orange")
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
    trim<-function(shape,type=NULL){
      if(type==1|type==3){return(t(t(shape[!rowSums(shape)==0,!colSums(shape)==0])))
      } else return(t(shape[!rowSums(shape)==0,!colSums(shape)==0]))
    } 
    
    if(shape==1){
      return(trim(L1shape(rotation),type = rotation))
    } else if(shape==2){
      return(trim(L2shape(rotation),type = rotation))
    } else if(shape==3){
      return(trim(lineshape(rotation),type = rotation))
    } else if(shape==4){
      return(trim(squareshape(rotation),type = rotation))
    } else if(shape==5){
      return(trim(N1shape(rotation),type = rotation))
    } else if(shape==6){
      return(trim(N2shape(rotation),type = rotation))
    } else if(shape==7){
      return(trim(Tshape(rotation),type = rotation))
    } else "sorry wrong shape"
  }
  
  
  startshape<-function(){
    shapenum<-sample(1:7,1)
    posnum<-sample(1:4,1)
    shp<-t(t(shapes(shapenum,posnum)))
    xrand<-sample(1:width,1)
    cnt<-0
    
    while((!length(xrand:width)>=ncol(shp)) & cnt<100){
      xrand<-sample(1:width,1);cnt=cnt+1
    }
    return(list(gridpos=xrand,shape=shp,shapenum=shapenum,posnum=posnum))
  }
  
  sts<-startshape()
  while(!any(data[1:dim(sts$shape)[1],sts$posnum:(sts$posnum+dim(sts$shape)[2]-1)]==1)){
    data1<-data
    update.plot(length = length,width=width,data=data)
    sts<-startshape()
    tym<-1
    while(tym!=22){
      data1<-data
      cvec<-sts$shape
      cvec[cvec==1]<-cols[sts$shapenum]
      cvec[cvec==0]<-data[tym:(tym+dim(sts$shape)[1]-1),sts$gridpos:(sts$gridpos-1+dim(sts$shape)[2])][cvec==0]
      data1[tym:(tym+dim(sts$shape)[1]-1),sts$gridpos:(sts$gridpos-1+dim(sts$shape)[2])]<-cvec
      update.plot(length = length,width = width,data=data1)
      tym=tym+1
      Sys.sleep(0.25)
    }
  }
  
  
}
