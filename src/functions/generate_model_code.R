generate_model_code = function(data) {
    copy(data)
    
    data[, model_code := paste(store_id, dept_id, sep = '_')]
    
    return(data)
}
