import os
import pandas as pd
from tqdm import tqdm


def SortAndCopy(results,outdir):
  print("Copying files...")
  for image in tqdm(results):
    
    source = Path(image["FilePath"])
    base = image["FileName"]
        
    classfolder = outdir + '/' + image["species"] + '/'
    #create classfolder if it doesnt yet exist  
    if not os.path.isdir(Path(classfolder)): os.makedirs(Path(classfolder))    
       
    date = image['FileModifyDate'].strftime('%Y-%m-%d')
    
    dest = Path(classfolder + date + "_" + base)
    
    if os.path.isfile(source):     
      if os.path.isfile(dest):
        print("File already exists! ",dest)
        pass
      else: shutil.copy2(source, dest)
    else: print(f"Error: File {source} not found!")


def SortAndCopy_MD(results):
  print("Copying files...")
  for image in tqdm(results):
      
    base = os.path.basename(image["file"])
  
    detex = image.get('max_detection_conf',-1)
    
    if detex < 0: continue #error
  
    elif detex > 0 : #object detected
        best = image['detections'][0]
        dest = args.output_dir + '/' + classes[int(best["category"])] + '/'
        
    else: # object not detected 
        dest = args.output_dir + '/' + classes[0] + '/'
        
    if not os.path.isdir(Path(dest)): os.makedirs(Path(dest))    
   
    source = image["file"]
  
    source = Path(source)
  
    dest = Path(dest + image['date'] + "_" + base)
    if os.path.isfile(source):     
        if os.path.isfile(dest):
            print("File already exists! ",dest)
            pass
        else: shutil.copy2(source, dest)
    else:
        print(source)
        print("File not found!")
