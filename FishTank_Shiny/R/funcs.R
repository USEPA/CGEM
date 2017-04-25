# a caller for state variable labels
labels_fun <- function(){
  
  library(ncdf4)
  
  if(!file.exists('NETCDF/output.000000.nc'))
    stop('Run model to get labels')

  # get model output from netcdf
  nc <- nc_open('NETCDF/output.000000.nc')
  
  # shrt names, all other vectors will be ordered using shrt
  shrt <- sort(names(nc$var))

  # long names
  lngs <- sapply(shrt, function(x) ncatt_get(nc, x, 'description')$value)
  lngs <- lngs[shrt]
  
  # units for each variable
  vals <- sapply(shrt, function(x) ncatt_get(nc, x, 'units')$value)
  vals <- vals[shrt]
  
  # remove names
  names(lngs) <- NULL
  names(vals) <- NULL
  
  out <- list(shrt = shrt, lngs = lngs, vals = vals)
  return(out)

}

# convert the initial conditions between formats
#
# parsin can be a chr string of file location of original InitialConditions.txt or input data frame of input conditions to be converted to ASCII format, the data frame is the output from the ASCII text file, but there's an additional function that replaces parameters in the data frame for conversion to the standard GEM format
forminps <- function(parsin){
 
  library(dplyr)
  
  # data frame to input file  
  if(inherits(parsin, 'data.frame')){
    
    # combine for easy write with writelines
    out <- with(parsin, paste(value, parm, sep = '\t!'))
    
  }
  
  # input file to data frame
  if(inherits(parsin, 'character')){
    
    # sanity checks
    if(!file.exists(parsin)) stop('Input file does not exist')

    # convert to data frame
    out <- readLines(parsin) %>% 
      gsub('\t+!|\\s+!', '\t', .) %>% # replace strings inbetween value and variable name with tab
      gsub('\\s+$', '', .) %>% # remove trailing spaces
      strsplit('\t') %>% 
      do.call('rbind', .) %>% 
      data.frame(., stringsAsFactors = FALSE)
    names(out) <- c('value', 'parm')
  
  }
  
  return(out)
  
}

# convert the input parameter info between formats
#
# parsin can be a chr string of file location of original GEM_InputFile or input data frame of parameters to be converted to ASCII format, the data frame is the output from the ASCII text file, but there's an additional function that replaces parameters in the data frame for conversion to the standard GEM format
formpars <- function(parsin){
 
  library(dplyr)
  
  # data frame to input file  
  if(inherits(parsin, 'data.frame')){

    # split data frame by parameters
    # reorganize duplicates as single chr vector
    # back to data frame
    tmp <- mutate(parsin, 
        parm = gsub('_[0-9]*$', '', parm),
        parm = factor(parm, levels =  unique(parm))
      ) %>% 
      split(., .$parm) %>% 
      lapply(., function(x) paste(x[1][, 1, drop = TRUE], collapse = ' ')) %>% 
      reshape2::melt(.) %>% 
      rename(parm = L1) %>% 
      mutate(
        ord = as.numeric(row.names(.)), 
        parm = paste0('!', parm), 
        value = paste0(value, '\t\t')
        )

    # add category labels and NA rows above
    # added by index but with decimal change for ordering
    cats <- c('Simulation Specifics', 'Switches in GEM', 'Optics', 'Temperature', 'Phytoplankton, up to 6 types', 'Zooplankton, up to 2 types', 'Organic Matter', 'River Loads - used in 3D only', 'Other including Boundary Conditions') 
    cats <- paste0('!', cats)
    locs <- grep('starting time|^!Which_fluxes|^!Kw$|^!Tref|^!ediblevector\\(Z1|^!Zeffic|^!KG1$|^!rcNO3|^!Which_VMix', tmp$parm, ignore.case = F)
    locs <- locs - 0.01
    lab <- data.frame(value = cats, parm = '', ord = locs)
    labfill <- data.frame(value = '', parm = '', ord = locs - 0.01)
    
    tmp <- rbind(tmp, lab, labfill) %>% 
      arrange(ord) %>% 
      select(-ord)
      
    # combine columns to vector
    out <- paste(tmp$value, tmp$parm, sep = '')
      
  }
  
  # input file to data frame
  if(inherits(parsin, 'character')){
    
    # sanity checks
    if(!file.exists(parsin)) stop('Input file does not exist')

    # readby lines, one list element per parameter 
    tmp <- readLines(parsin) %>% 
      strsplit('!') %>% 
      lapply(., function(x){
        tospl <- x[1] %>% 
          gsub('\t', '', .) %>%
          strsplit(' ') %>% 
          .[[1]] %>% 
          .[nchar(.) > 0] %>% 
          list(., x[2])
        tospl
      })

    # get parameter names, everything left of colon
    nms <- lapply(tmp, function(x) x[[2]]) %>% 
      unlist %>% 
      gsub(':.*$', '', .)
    
    # get parameter values, rename list elements as parameter names
    tmp <- lapply(tmp, function(x) x[[1]]) 
    names(tmp) <- nms

    # list elements that contain parameters (inverse of notparms)
    cats <- c('Simulation Specifics', 'Switches in GEM', 'Optics', 'Temperature', 'Phytoplankton, up to 6 types', 'Zooplankton, up to 2 types', 'Organic Matter', 'River Loads - used in 3D only', 'Other including Boundary Conditions') 
    notparms <- which(nms %in% cats)
    if(length(notparms) != length(cats)) stop('Cannot find all parameter categories in input file')
    notparms <- sort(c(as.numeric(which(is.na(tmp))), notparms))
    parms <- seq(length(tmp))
    parms <- parms[!parms %in% notparms]
    tmp <- tmp[parms] 

    # empties are important, need to convert to blank character
    tmp[unlist(lapply(tmp, length)) == 0] <- ' '
    
    # suffix for parameters with more than one value
    suff <- lapply(tmp, function(x) seq(length(x))) %>% 
      unlist #%>% 
      # gsub('0', '1', .) # the blank rows are important
    
    # melt list, add suffix to id more than one parameter value
    out <- reshape2::melt(tmp) %>% 
      mutate(
        L1 = paste0(L1, '_', suff),
        value = as.character(value)
        ) %>% 
      rename(parm = L1)
    
  }
  
  return(out)
  
}

# create file for initial conditions, defaults to existing RData object is inps = NULL, otherwise values are replaced
# 
# inps is a named list where each element is one to many parameter values for each input condition
# partial string matching is used for the names to replace values in a default input list
# passing NULL to inps will return the existing input conditions file
setinps <- function(inps = NULL){
   
  library(dplyr)
  
  # load default parameter file
  load('input/InitialConditions.RData')
  
  # copy default parameter file in CGEM format if no new parameters found
  if(is.null(inps)){
    
    out <- forminps(InitialConditions)
   
  # replace row values in input file with input list
  } else {
    
    # format parm names in input list for matching with new parm names
    # must remove regex metacharacters 
    inp_nms <- gsub('\\+|\\(|\\)', '', InitialConditions$parm)

    # replace each parameter with new
    for(inp in names(inps)){
      
      # index in defaults to replace
      sel <- gsub('\\+|\\(|\\)', '', inp) %>% 
        paste0('^', .) %>% 
        grep(., inp_nms)
      
      # stop if name is not matched in defaults
      if(length(sel) == 0) stop('No matches for initial conditions in ', inp)
      if(length(sel) > 1){
        mtchs <- paste(GEM_InputFile[sel, 'parm'], collapse = ', ')
        stop('Multiple matches for inital conditions in ', inp, ': ', mtchs)
      }
      
      InitialConditions[sel, 'value'] <- as.character(inps[[inp]])
      
    }
    
    # format new file for export
    out <- forminps(InitialConditions)
    
  }
  
  # save output to file
  writeLines(out, 'input/InitialConditions.txt')
  
}

# create parameter file for input with new parameters, uses formpars to change defaults
# 
# pars is a named list where each element is one to many parameter values for each parameter
# partial string matching is used for the element names to replace values in a default parameter list
# passing NULL to pars will return the default parameter list for Weeks Bay
setpars <- function(pars = NULL){
  
  library(dplyr)
  
  # load default parameter file
  load('input/GEM_InputFile.RData')
  
  # copy default parameter file in CGEM format if no new parameters found
  if(is.null(pars)){

    out <- formpars(GEM_InputFile)
   
  # replace row values in input file with input list
  } else {

    # format parm names in input list for matching with new parm names
    # must remove regex metacharacters 
    par_nms <- gsub('\\+|\\(|\\)', '', GEM_InputFile$parm)
    
    # replace each parameter with new
    for(par in names(pars)){
      
      # index in defaults to replace
      sel <- gsub('\\+|\\(|\\)', '', par) %>% 
        paste0('^', .) %>% 
        grep(., par_nms)
      
      # stop if name is not matched in defaults
      if(length(sel) == 0) stop('No parameter matches for ', par)
      if(length(sel) > 1){
        mtchs <- paste(GEM_InputFile[sel, 'parm'], collapse = ', ')
        stop('Multiple parameter matches for ', par, ': ', mtchs)
      }
      
      GEM_InputFile[sel, 'value'] <- as.character(pars[[par]])
      
    }
    
    # format new file for export
    out <- formpars(GEM_InputFile)
    
  }

  # save output to file
  writeLines(out, 'input/GEM_InputFile')
  
}

# get chr string of parameter names that can be replaced
showpars <- function(){
  
  fl <- 'input/GEM_InputFile.RData'
  if(!file.exists(fl))stop(fl, ' does not exist')
  
  load(file = fl)
  
  out <- GEM_InputFile$parm
  
  return(out)
  
}

# run the model
# copies intial conditions and parameter values from input
# executes model
# formats output to return data frame
#
# out_var is chr string of variable to return
# p1z1 logical if only one phyto and one zoop group are used, passed to p1z1_swtch
run_mod <- function(pars = NULL, inps = NULL, out_var = 'O2', p1z1 = FALSE){
  
  library(ncdf4)
  
  # create parameter file based on inputs
  setpars(pars)
  
  # create initial conditions file based on inputs
  setinps(inps)

  # move the input files from input to root
  fls <- c('input/InitialConditions.txt', 'input/GEM_InputFile')
  file.copy(fls, getwd(), overwrite = TRUE)
  
  # set to one phyto, one zoop group if TRUE
  if(p1z1) p1z1_swtch(to = TRUE)
  
  # run model, suppress output messages
  suppressWarnings(system('FishTank.exe'))
  
  # back to default six phyt, two zoop if TRUE
  if(p1z1) p1z1_swtch(to = FALSE)
  
  # remove temp files
  file.remove(c('input/GEM_InputFile', 'input/InitialConditions.txt'))
  
  # get model output from netcdf
  nc <- nc_open('NETCDF/output.000000.nc')

  # return all if out_var is NULL
  if(is.null(out_var)) return(nc)

  # otherwise subset
  if(length(out_var) > 1) stop('only one value can be passed to out_var')
  
  # get all relevant organic matter variables for OM1 or OM2
  if(out_var %in% c('OM1', 'OM2')){

    # need to sum the OM variables
    out_var_OM <- paste(out_var, c('A', 'BC', 'R', 'Z'), sep = '_')
    sel <- names(nc$var) %in% out_var_OM
    if(sum(sel) != 4) stop('Check hard-coded organic variables')
    
    # get each variable, take rowsums
    var <- sapply(out_var_OM, function(x) ncvar_get(nc, x)[1, ]) %>% 
      rowSums

  } else {

    sel <- names(nc$var) %in% out_var
    if(sum(sel) == 0) stop(paste(out_var, 'not found in model output'))
    
    # get variable
    var <- ncvar_get(nc, out_var)[1, ]
    
  }

  # get time, add output variable
  time <- ncvar_get(nc, "time")
  out <- data.frame(time, var)
  names(out) <- c('time', out_var)
    
  return(out)
  
}

# the plotting function
# varsel is input name, all dat is input data
plo_fun <- function(varsel, alldat, logscale = FALSE){

  library(ncdf4)
  library(dygraphs)

  # get short label from long
  varsel <- labels_fun()$shrt[labels_fun()$lngs %in% varsel]
  
  # units and variable name
  units <- ncatt_get(alldat, varsel, attname = 'units')$value
  descr <- ncatt_get(alldat, varsel, attname = 'description')$value

  # format unit labels
  units <- gsub('-', ' ', units)
  units <- HTML(gsub('/m3', ' m<sup>-3</sub>', units))
  units <- HTML(gsub('/cm2/s$', ' cm<sup>-2</sup> s<sup>-1</sup>', units))   
  units <- HTML(gsub('/cell$', ' cell<sup>-1</sup>', units))   
  units <- HTML(gsub('^d 1$', 'd<sup>-1</sup>', units))   
   
  # get variable, time steps
  var <- ncvar_get(alldat, varsel)[1, ]
  time <- ncvar_get(alldat, "time")
  out <- data.frame(time, var)
  names(out) <- c('time', descr)
  
  # format the time series
  yr <- ncatt_get(alldat, 0, 'iYr0')$value
  mo <- ncatt_get(alldat, 0, 'iMonS')$value
  dy <- ncatt_get(alldat, 0, 'iDayS')$value
  hr <- ncatt_get(alldat, 0, 'iHrS')$value
  mn <- ncatt_get(alldat, 0, 'iMinS')$value
  sc <- ncatt_get(alldat, 0, 'iSecS')$value
  strt <- paste(paste(yr, mo, dy, sep = '-'), paste(hr, mn, sc, sep = ':'))
  strt <- as.POSIXct(strt, tz = 'GMT')
    
  # make xts
  step <- as.POSIXct(time, origin = strt) 
  toplo <- as.matrix(var)
  toplo <- as.xts(toplo, order.by = step)
  names(toplo) <- varsel
 
  # plot output, logscale if/then
  if(logscale){
      
    dygraph(toplo, group = 'group') %>% 
      dyRangeSelector %>% 
      # dyOptions(digitsAfterDecimal = 0) %>% 
      dyAxis('y', label = units, logscale = logscale)
    
  } else {
  
    dygraph(toplo, ylab = units, group = 'group') %>% 
      dyRangeSelector %>% 
      # dyOptions(digitsAfterDecimal = 0) %>% 
      dyAxis('y', label = units)

      
  }

}

# function for saving plot outputs
# vars_in is input variables to plot, all dat is input data
saveplo_fun <- function(vars_in, alldat, logspace = FALSE){

  library(ncdf4)
  library(ggplot2)

  for(var_in in vars_in){
    
    # get short label from long
    varsel <- labels_fun()$shrt[labels_fun()$lngs %in% var_in]
    
    # units and variable name
    units <- ncatt_get(alldat, varsel, attname = 'units')$value
    descr <- ncatt_get(alldat, varsel, attname = 'description')$value
  
    # get variable, time steps
    var <- ncvar_get(alldat, varsel)[1, ]
    time <- ncvar_get(alldat, "time")
    time <- as.POSIXct(time, origin = as.POSIXct('2002-01-01 0:0', tz = "GMT"), tz = 'GMT')
    out <- data.frame(time, var)
    
    plotheme <- theme_minimal() + 
      theme(
        plot.background = element_rect(fill='transparent', 
          colour = NA),
        panel.background = element_rect(fill='transparent', 
          colour = NA),
        legend.background = element_rect(fill='transparent', 
          colour = NA),
        legend.key = element_rect(fill = 'transparent', 
          colour = NA),
        axis.line.x = element_line(), 
        axis.line.y = element_line(), 
        axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(.1, "cm"), 
        axis.title.x = element_blank()
      )   
        
    p <- ggplot(out, aes(x = time, y = var)) + 
      geom_line() + 
      scale_y_continuous(units) + 
      plotheme + 
      ggtitle(descr)
    
    if(logspace) 
      p <- p + scale_y_continuous(units, trans = 'log')
    
    print(p)

  }
  
}

# formatting the labels from labels_fun as expressions for plots
expr_fun <- function(lab_in){
 
  sel <- which(labels_fun()$shrt == lab_in)
  val <- labels_fun()$vals[sel]
  
#   if(grepl('-', val)){
#     val <- strsplit(val, '-')[[1]]
#     val <- bquote(.(val[1]) ^ .(paste0('-', val[2])))
#   }
    
  return(val)
   
}


######
# format parameter inputs for shiny app, input is reactive object from shiny (user inputs from ui), output is list to send to setpars
# react_ls is created from reactiveValuesToList(input)
form_parinps <- function(react_ls, flrv){
  
  react_ls <- reactiveValuesToList(react_ls)

  # use input file if provided
  if(!is.null(flrv$data)){
    
    myfl <- formpars(flrv$data$datapath)
    out_ls <- as.list(myfl[, 1])
    names(out_ls) <- myfl[, 2]
    
  # otherwise use gui inputs
  } else {
    
    # format times argument separately
    times <- react_ls$times
    times <- list(
      '- starting time.*1' = as.numeric(format(times[1], '%Y')),
      '- starting time.*2' = as.numeric(format(times[1], '%m')),
      '- starting time.*3' = as.numeric(format(times[1], '%d')),
      '- ending   time.*1' = as.numeric(format(times[2], '%Y')),
      '- ending   time.*2' = as.numeric(format(times[2], '%m')),
      '- ending   time.*3' = as.numeric(format(times[2], '%d'))
      )
     
    # remove inputs that are not parameters, make sure this works
   
    # format parm names in input list for matching with new parm names
    # must remove regex metacharacters 
    inps <- gsub('\\+|\\(|\\)', '', names(react_ls))
    load('input/GEM_InputFile.RData')
    torm <- gsub('\\+|\\(|\\)*', '', GEM_InputFile$parm) %>% 
      paste0('^', .) %>% 
      paste(., collapse = '|') %>% 
      grep(., inps, value = T, invert = T)
    
    out_ls <- react_ls[!names(react_ls) %in% torm]
    out_ls <- c(out_ls, times)
    
  }
  
  return(out_ls)

}

######
# react_ls is created from reactiveValuesToList(input)
form_iniinps <- function(react_ls){
  
  react_ls <- reactiveValuesToList(react_ls)

  # select the initial condition values from the list
  load('input/InitialConditions.RData')
  tosel <- InitialConditions$parm
  out_ls <- react_ls[tosel]
  
  return(out_ls)

}

# change files in tree to run FishTank with one phyto and one zoop group
# changes the files data/Model_dim.txt, InitialConditions.txt, and GEM_InputFile 
# last two files are in root, created on the fly with run_mod from input file so no need to convert back if to = F
#
# to logical indicating if changes are made from default (six phyto, two zoop) to one each, set to F to go from one group each back to default
p1z1_swtch <- function(to = TRUE){
 
  # forward change
  if(to){
    
    # model dimensions file
    mod_dim <- readLines('data/Model_dim.txt')
    phytsel <- grep('nospA', mod_dim)
    zoopsel <- grep('nospZ', mod_dim)
    mod_dim[phytsel] <- gsub('^[6]', '1', mod_dim[phytsel])
    mod_dim[zoopsel] <- gsub('^[2]', '1', mod_dim[zoopsel])
    writeLines(mod_dim, 'data/Model_dim.txt')
    
    # InitialConditions, remove 2-6 phyto and 2 zoop
    inits <- readLines('InitialConditions.txt')
    rm <- grep('A[2-6]$|n[2-6]$|p[2-6]$|G[2]$', inits)
    inits <- inits[-rm]
    writeLines(inits, 'InitialConditions.txt')
    
    # GEM_InputFile
    gem_inp <- readLines('GEM_InputFile')
    sel <- grep('ediblevector\\(Z[2]\\)$', gem_inp)
    gem_inp <- gem_inp[-sel]
    writeLines(gem_inp, 'GEM_InputFile')
  
  # backward change
  # GEM_InputFile in root does not need to be changed back, overwritten every time model is run
  } else {

    # model dimensions file
    mod_dim <- readLines('data/Model_dim.txt')
    phytsel <- grep('nospA', mod_dim)
    zoopsel <- grep('nospZ', mod_dim)
    mod_dim[phytsel] <- gsub('^[1]', '6', mod_dim[phytsel])
    mod_dim[zoopsel] <- gsub('^[1]', '2', mod_dim[zoopsel])
    writeLines(mod_dim, 'data/Model_dim.txt')
    
  }
 
  return(NULL)
  
}