import pandas as pd
import os
import glob
from PIL import Image, ExifTags
from datetime import datetime

def load_image(input_file):
    """
    from CameraTraps/visualization/visualization_utils.py
    """
    image = Image.open(input_file)
    if image.mode not in ('RGBA', 'RGB', 'L'):
        raise AttributeError('Input image {} uses unsupported mode {}'.format(input_file, image.mode))
    if image.mode == 'RGBA' or image.mode == 'L':
        # PIL.Image.convert() returns a converted copy of this image
        image = image.convert(mode='RGB')
    image.load()
    return image


def is_image_file(s):
    """
    from CameraTraps/MegaDetector
    Check a file's extension against a hard-coded set of image file extensions    '
    """
    image_extensions = ['.jpg', '.jpeg', '.gif', '.png', '.mp4']
    ext = os.path.splitext(s)[1].lower()
    return ext in image_extensions


def buildFileManifest(imagedir, outfile = None):
    if outfile: pass
        #load file manifest 
    if not os.path.isdir(imagedir):
        return("The given directory does not exist.")

    files = glob.glob(os.path.join(imagedir, '**', '*.*'), recursive=True)
    images = [s for s in files if is_image_file(s)]

    images = pd.DataFrame(images, columns = ["FilePath"])
    images["FileName"] = images["FilePath"].apply(lambda x: os.path.split(x)[1])
    images["FileModifyDate"] = images["FilePath"].apply(lambda x: datetime.fromtimestamp(os.path.getmtime(x)).strftime('%Y-%m-%d %H:%M:%S'))

    return(images)
