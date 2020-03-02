cat('\nLoading packages:\n\n')

.libPaths(.Library)
# source functions
sapply(list.files("R", full.names = TRUE), source)

if (!'package:dplyr' %in% search()) {
    # initialize VC_library
    .libPaths(.Library)
    if (nchar(Sys.getenv('R_VC_LIBRARY_LOCATION')) > 0) { # Load library functionallity:
        library("multiversion", character.only = T, lib.loc = paste(Sys.getenv('R_VC_LIBRARY_LOCATION'), 'multiversion/0.1.0', sep = '/'))
    } else {
        stop('No environment variable has been set for me to find the R_VC_library location. Please fill the environment variable `R_VC_LIBRARY_LOCATION` (ask siete).')
    }
    
    # load all other packages
    lib.load(MASS, dplyr, tidyr, ggplot2, cli, utf8, crayon, bindrcpp, labeling, DT, lazyeval, appendLibPaths =T, quietly = TRUE, pick.last = TRUE)
}
