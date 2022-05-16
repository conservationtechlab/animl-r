import pandas as pd
import os
import glob
import cv2
import random
from PIL import Image
from datetime import datetime
import multiprocessing as mp

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

def is_image(s):
    """
    from CameraTraps/MegaDetector
    Check a file's extension against a hard-coded set of image file extensions    '
    """
    image_extensions = ['.jpg', '.jpeg', '.gif', '.png', '.mp4']
    ext = os.path.splitext(s)[1].lower()
    return ext in image_extensions

def extractImages(file_path, outdir, fps=None, frames=None):

    cap = cv2.VideoCapture(file_path)
    filename = os.path.basename(file_path)
    filename, extension = os.path.splitext(filename)
    frames_saved = []

    if fps is None:
        video_fps = cap.get(cv2.CAP_PROP_FPS)
        frame_count = int(cap.get(cv2.CAP_PROP_FRAME_COUNT))
        duration = frame_count / video_fps
        frame_capture = 0
        increment = int(frame_count / frames)
        while cap.isOpened() and (len(frames_saved) < frames):
            cap.set(cv2.cv2.CAP_PROP_POS_FRAMES, frame_capture)
            ret, frame = cap.read()
            if not ret:
                break

            out_path = outdir + filename + "-" + '{:05}'.format(random.randrange(1, 10 ** 5)) + "-" + str(
                frame_capture) + '.jpg'
            cv2.imwrite(out_path, frame)
            frames_saved.append(out_path)
            frame_capture += increment
    else:
        frame_capture = 0
        while cap.isOpened():
            ret, frame = cap.read()
            cap.set(cv2.CAP_PROP_POS_MSEC, (frame_capture * 1000))
            if not ret:
                break

            out_path = outdir + filename + "-" + '{:05}'.format(random.randrange(1, 10 ** 5)) + "-" + str(
                frame_capture) + '.jpg'
            cv2.imwrite(out_path, frame)
            frames_saved.append(out_path)
            frame_capture += fps

        cap.release()
        cv2.destroyAllWindows()

    return frames_saved


def imagesFromVideos(files, outdir=None, outfile=None, format="jpg", fps=None, frames=None, parallel=False, nproc=1):
    # assert type(files) == "DataFrame", "'files' must be Data Frame."
    if not os.path.isdir(outdir): os.makedirs(outdir)

    if (fps != None) and (frames != None):
        print("If both fps and frames are defined fps will be used.")
    assert (fps != None) or (frames != None), "Either fps or frames need to be defined."
    images = []
    videos = []
    for file in files:
        filename, extension = os.path.splitext(file)
        extension = extension.lower()
        if extension == 'jpg' or extension == 'png':
            images.append(file)
        else:
            videos.append(file)

    if parallel:
        cpu_count = mp.cpu_count()
        pool = mp.Pool(cpu_count)
        videoframes = [pool.apply(extractImages, args=(video, outdir, fps, frames)) for video in videos]
        pool.close()

    else:
        videoframes = []
        for video in videos:
            videoframes.append(extractImages(video, outdir=outdir, fps=fps, frames=frames))
    return videoframes


def buildFileManifest(imagedir, outfile=None):
    if outfile: pass
    # load file manifest
    if not os.path.isdir(imagedir):
        return ("The given directory does not exist.")

    files = glob.glob(os.path.join(imagedir, '**', '*.*'), recursive=True)
    images = [s for s in files if is_image(s)]

    images = pd.DataFrame(images, columns=["FilePath"])
    images["FileName"] = images["FilePath"].apply(lambda x: os.path.split(x)[1])
    images["FileModifyDate"] = images["FilePath"].apply(
        lambda x: datetime.fromtimestamp(os.path.getmtime(x)).strftime('%Y-%m-%d %H:%M:%S'))

    return (images)
