#functions for project-measure


##############################################################################
##### helper functions #####

# converts inches to cm
convert.inchestocm = function()
  {
  skip = c("minutes", "age", "quality");
  ## converting everything gto cm
  for(r in 1:nrow)
    {
      row = measure[r,];
      mynames = names(row);
      if(row$my.units == "in")
      {
        for(c in 1:ncol)
        {
          myname = mynames[c];
          if(!is.element(myname, skip))
          {
            myval = unlist(row[c]);
            print(paste0("r: ",r, " c: ", c," ... old: ",myval, "  ", is.numeric(myval), "   "));
            if(is.numeric(myval))
            {
              newval = myval * 2.54; # 2.54 cm = 1 in
              measure[r,c] = newval;
              print(paste0(" --> ",newval));
            }
          }
        }
      }
    }
  }

#merge the data with right and left measure into one average value

merge.left.right = function(measure,getOne)
  {
  n.rows = dim(measure)[1];
  for(one in getOne)
    {
      measure[one] = NA;
    }
    
  for(i in 1:n.rows)
    {  
      measure.row = measure[i,];
      for(one in getOne)
      {
        nidx = getIndexOfDataFrameColumns(measure, one);
        
        myleft = paste0(one,".left");
        lidx = getIndexOfDataFrameColumns(measure.row, myleft);
        myright = paste0(one,".right");
        ridx = getIndexOfDataFrameColumns(measure.row, myright);
        
        print(paste0(
          "left: ",myleft," --> ",lidx,
          " right: ",myright," --> ",ridx
        )
        );
        
        row.m = mean(
          c(as.numeric(unlist(measure.row[lidx])),
            as.numeric(unlist(measure.row[ridx]))),
          na.rm=TRUE);
        
        measure[i,nidx] =  row.m;
      }
    }
  }

# scale the variables by scaling variable

build.scale.variables = function(df, colnum, new.colname, scalevar )
{
  n=1
  for(name in new.colname)
  {
    df[name] = NA;
  }
  
  for(i in colnum)
  {
    scaled.i<-df[,i]/scalevar
    df[new.colname[n]]<-scaled.i
    n = n+1
  }
  df
}
