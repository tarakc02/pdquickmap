#' Set or get an API key
#' 
#' Reads (or writes) an API key to an environment variable, and stores the key 
#' to disk.
#' 
#' @param provider Which service is this a key for? 
#' @param force Overwrite existing key? If TRUE, the user will be prompted to 
#'  enter a user key even if one has already been entered before.
#' @export
api_key <- function(provider, force = FALSE) {
    key_name <- paste(toupper(provider), "KEY", sep="_")
    
    # if the key is already loaded, we're done
    env <- Sys.getenv(key_name)
    if (!identical(env, "") && !force) 
        return(env)
    
    # this is where the file does/will live
    file_name <- paste(system.file("extdata", package="pdquickmap"), 
                       "/", key_name, sep="")
    
    # if the file exists, just read the key from there and quietly set the 
    # environment variable
    if (file.exists(file_name) && !force) {
        key <- readRDS(file_name)
        args = list(key)
        names(args) = key_name
        do.call(Sys.setenv, args)
        return(key)
    }
    
    if (!interactive()) {
        stop(paste("Please set env var", key_name, "to your", provider, "API key"), 
             call. = FALSE)
    }
    
    key <- readline(paste("Please enter your", provider, "API key: "))
    
    if (identical(key, "")) {
        stop(paste(provider, "API key entry failed"), call. = FALSE)
    }
    
    message(paste("Updating", key_name, "environment variable"))
    args = list(key)
    names(args) = key_name
    do.call(Sys.setenv, args)
    saveRDS(key, file_name)
    key
}