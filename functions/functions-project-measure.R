#functions for project-measure


##############################################################################
##### helper functions #####

# converts inches to cm
convert.inchestocm = function(df)
  {
  measure <-df
  myUnits = tolower(measure$units);
  myUnits[myUnits=="inches"] = "in";
  myUnits[myUnits=="inch"] = "in";
  myUnits[myUnits=="\"in\""] = "in";
  measure$my.units = myUnits;
  
  nrow = nrow(measure);
  ncol = ncol(measure);
  skip = c("minutes", "age", "quality");
  ## converting everything to cm
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
            # print(paste0("r: ",r, " c: ", c," ... old: ",myval, "  ", is.numeric(myval), "   "));
            if(is.numeric(myval))
            {
              newval = myval * 2.54; # 2.54 cm = 1 in
              measure[r,c] = newval;
              #print(paste0(" --> ",newval));
            }
          }
        }
      }
    }
  measure
  }


#merge the data with right and left measure into one average value

merge.left.right = function(df,getOne)
  {
  measure <-df
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
        
        # print(paste0(
        #   "left: ",myleft," --> ",lidx,
        #   " right: ",myright," --> ",ridx
        # )
        # );
        
        row.m = mean(
          c(as.numeric(unlist(measure.row[lidx])),
            as.numeric(unlist(measure.row[ridx]))),
          na.rm=TRUE);
        
        measure[i,nidx] =  row.m;
      }
    }
  measure
  }


# scale the variables by scaling variable

build.scale.variables = function(df, colnum, new.colname, scalevar )
{
  n=1
  new.df <-df
  for(name in new.colname)
  {
    new.df[name] = NA;
  }
  
  for(i in colnum)
  {
    new.df[new.colname[n]]<-new.df[,i]/scalevar
    n = n+1
  }
  new.df
}

prepareMeasureData = function(df)
{
  measure <-df
  # clean up the variable names
  myEthnicity = tolower(measure$ethnicity);
  myEthnicity[myEthnicity=="white"] = "w";
  myEthnicity[myEthnicity=="caucasian"] = "w";
  myEthnicity[myEthnicity=="caucasain"] = "w";
  myEthnicity[myEthnicity=="\"caucasian\""] = "w";
  myEthnicity[myEthnicity=="anglo"] = "w";
  myEthnicity[myEthnicity=="white non-hispanic"] = "w";
  myEthnicity[myEthnicity=="black"] = "b";
  myEthnicity[myEthnicity=="african american"] = "b";
  myEthnicity[myEthnicity=="hispanic"] = "h";
  myEthnicity[myEthnicity=="japanese"] = "a";
  myEthnicity[myEthnicity=="filipino"] = "pi";
  myEthnicity[myEthnicity=="native american"] = "nat";
  myEthnicity[myEthnicity=="latin american"] = "l";
  myEthnicity[myEthnicity=="latino"] = "l";
  myEthnicity[myEthnicity=="\"asian/latino\""] = "al";
  myEthnicity[myEthnicity=="white-filipino"] = "wf";
  myEthnicity[myEthnicity=="asain"] = "a";
  myEthnicity[myEthnicity=="asian"] = "a";
  myEthnicity[myEthnicity=="\"asian\""] = "a";
  myEthnicity[myEthnicity=="chinese"] = "a";
  myEthnicity[myEthnicity=="laotian"] = "a";
  myEthnicity[myEthnicity=="caucasian/asian"] = "ca";
  myEthnicity[myEthnicity=="white italian"] = "w";
  myEthnicity[myEthnicity=="japanese italian"] = "ji";
  myEthnicity[myEthnicity=="pacific islander"] = "pi";
  myEthnicity[myEthnicity=="korean"] = "a";
  myEthnicity[myEthnicity=="indian"] = "a";
  measure$my.ethnicity = myEthnicity;
  myGender = tolower(measure$gender);
  myGender[myGender=="female"] = "f";
  myGender[myGender=="male"] = "m";
  myGender[myGender=="\"male\""] = "m";
  myGender[myGender=="\"female\""] = "f";
  myGender[myGender=="non-binary"] = "o";
  measure$my.gender = myGender;
  
  measure$new.units = "cm";
############################################################
  # Save measue rds 
  saveRDS(measure,"measure.rds");
  measure.short = measure[,c(1,2,4:6,17,24,27:50)];
  myNames = gsub(".NA","", colnames(measure.short), fixed=TRUE);
  colnames(measure.short) = myNames;
  colnames(measure.short)[11] = "eye.color";
  # get rid of duplicated tuples and save as rds
  measure.df = removeDuplicatesFromDataFrameAllColumns(measure.short);
  saveRDS(measure.df,"measure.df.rds");
  
  # create final df, delete rows for specific collector and person_id for the data integrity 
  final.df = measure.df;
  final.df = subsetDataFrame(final.df, "data_collector", "!=", "411f8b5c9500b7cc928c93dd1a0006b7");
  final.df = subsetDataFrame(final.df, "person_id", "!=", "81a9f3915f64dc6edd329b89bea87d5e");
  final.df = subsetDataFrame(final.df, "person_id", "!=", "\"ee33e909372d935d190f4fcb2a92d542\"");
  
  # rename values and categorize certain attributes
  myEye = tolower(final.df$eye);
  myEye[myEye == "both"] = "b";
  myEye[myEye == "\"both\""] = "b";
  myEye[myEye == "equal"] = "b";
  myEye[myEye == "right"] = "r";
  myEye[myEye == "\"right\""] = "r";
  myEye[myEye == "left"] = "l";
  myEye[myEye == "\"left\""] = "r";
  myEye[myEye == "brown"] = NA;
  final.df$my.eye = factor(myEye);
  myWriting = tolower(final.df$writing);
  myWriting[myWriting == "right"] = "r";
  myWriting[myWriting == "\"right\""] = "r";
  myWriting[myWriting == "left"] = "l";
  myWriting[myWriting == "both"] = "b";
  final.df$my.writing = factor(myWriting);
  mySwinging = tolower(final.df$swinging);
  mySwinging[mySwinging == "right"] = "r";
  mySwinging[mySwinging == "rigth"] = "r";
  mySwinging[mySwinging == "\"right\""] = "r";
  mySwinging[mySwinging == "left"] = "l";
  mySwinging[mySwinging == "let"] = "l";
  mySwinging[mySwinging == "leftt"] = "l";
  mySwinging[mySwinging == "both"] = "b";
  final.df$my.swinging = factor(mySwinging);
  myEyeColor = tolower(final.df$eye.color);
  myEyeColor[myEyeColor == "brown"] = "br";
  myEyeColor[myEyeColor == "\"brown\""] = "br";
  myEyeColor[myEyeColor == "blue"] = "bl";
  myEyeColor[myEyeColor == "\"blue\""] = "bl";
  myEyeColor[myEyeColor == "green"] = "gr";
  myEyeColor[myEyeColor == "hazel"] = "ha";
  myEyeColor[myEyeColor == "grey"] = "g";
  myEyeColor[myEyeColor == "blue-green"] = "bl-gr";
  myEyeColor[myEyeColor == "blue/green"] = "bl-gr";
  myEyeColor[myEyeColor == "blue-grey"] = "bl-g";
  myEyeColor[myEyeColor == "grey-green"] = "gr-g";
  myEyeColor[myEyeColor == "black"] = "b";
  myEyeColor[myEyeColor == "left"] = NA;
  final.df$my.eye.color = factor(myEyeColor);
  final.df$my.gender = factor(final.df$my.gender);
  final.df$my.ethnicity = factor(final.df$my.ethnicity);
  # measure.nic = subsetDataFrame(measure.short, "data_collector", "==", "0185c7c2eed9d48197953305a817c8b1");
  # measure.nic2 = subsetDataFrame(measure.df, "data_collector", "==", "0185c7c2eed9d48197953305a817c8b1");
  # 411f8b5c9500b7cc928c93dd1a0006b7
  final.df$person_id = trimMe( gsub('"',"",final.df$person_id,fixed=TRUE) );
  # save final.measure rds
  saveRDS(final.df,"final.measure.rds");
  utils::write.table(final.df, file="final.measure.txt", quote=FALSE, col.names=TRUE, row.names=FALSE, sep="|");
  final.df
}