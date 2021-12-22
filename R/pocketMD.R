#' Run MD only, create new folders for the 4 classes, and copy files
#'
#' @param sourcedir
#' @param outdir
#' @param checkpoint
#' @param resume_checkpoint
#' @param resume_results
#'
#' @return
#' @export
#'
#' @examples
#'
#
# pocketMD <- function(sourcedir,outdir,checkpoint = 2500,resume_checkpoint = NULL,resume_results = FALSE){
#   classes <- c("Empty","Animal","Human","Vehicle")
#   output_file <- "MD_results.json"
#   if(!dir.exists(outdir)){mkdir(outdir)}
#
#   #start from scratch
#   if(!resume_results){
#     if(paste0(outdir,output_file)){
#       print("Warning: output file already exists and will be overwritten")
#     }
#
#     # load the checkpoint if available
#     if(file.exists(resume_checkpoint)){
#         if(tolower(readline(prompt="Results file exists, would you like to resume? y/n: "))=="y"){
#           load(resultsfile)
#           images<-images[!(images %in% sapply(results,function(x)x$file))]
#           cat(length(results),"records loaded.\n")
#         }else{
#           results<-list()
#         }
#     }
#     else{results<-list()}
#
#     # find the images to score; a directory may need to recurse
#     if(dir.exists(sourcedir)){
#       image_file_names = ImagePathUtils.find_images(sourcedir)
#       sprintf("%i image files found in the input directory",len(image_file_names))
#     }
#
#     else{ stop("image_dir specified is not a directory (or does not have recognizable extensions), exiting.")}
#
#     if(length(image_file_names) > 0){stop("image_file provided does not point to valid image files")}
#     if(file.exists(image_file_names[1])){stop("The first image to be scored does not exist")}
#
#     # test that we can write to the output_files dir if checkpointing requested
#     if(checkpoint != -1){
#       checkpoint_path = paste0(outdir, "checkpoint_{}.json".format(datetime.utcnow().strftime("%Y%m%d%H%M%S")))
#       with open(checkpoint_path, "w") as f:
#         json.dump({"images": []}, f)
#       print("The checkpoint file will be written to {}".format(checkpoint_path))
#     }
#     else{checkpoint_path <- NULL}
#
#     #load MD model, classify images
#     mdsession<-loadMDModel(mdmodel)
#
#     mdres<-classifyImagesBatchMD2(mdsession,images$FilePath,resultsfile=resume_checkpoint,checkpoint = 2500)
#     mdresflat<-flattenBoxesMDSimple(mdres)
#
#     #save output just in case
#     with open(args.output_dir + "/" + output_file, "w") as f:
#       json.dump(mdresflat, f, indent=1)
#
#     write.csv(mdresflat,paste0(datadir,cropfile),row.names=F,quote = F)
#     save(mdres,file=mdresults)
#   }
#
#   #load results
#   else{
#     checkpoint_path <- NULL
#     print("Loading results...")
#     results = load(paste0(outdir,output_file))
#   }
#
#   print("Copying files...")
#
#   #reorganize
#   for image in tqdm(results):
#
#     base = os.path.basename(image["file"])
#
#   detex = image.get("max_detection_conf",-1)
#
#   # null image
#   if detex < 0:
#     continue
#
#   else if detex > 0 : #object detected
#     best = image["detections"][0]
#   dest = args.output_dir + "/" + classes[int(best["category"])] + "/"
#
#   # empty detected
#   else:
#     dest = args.output_dir + "/" + classes[0] + "/"
#
#   if not os.path.isdir(Path(dest)): os.makedirs(Path(dest))
#
#   source = image["file"]
#
#   source = Path(source)
#   dest = Path(dest + image["date"] + "_" + base)
#
#   #copy files to sorted destination folder
#   if(file.exists(source)){
#     if(file.exists(dest){
#       print("File already exists!")
#       next
#     }
#     else{shutil.copy2(source, dest)}
#   }
#   else{
#     print(source)
#     print("Error, file not found!")
#   }
#   print("Done!")
# }
# '''
