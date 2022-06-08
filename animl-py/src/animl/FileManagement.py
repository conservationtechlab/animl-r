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


def parseMD(results):
    if len(results) > 0:
        df = pd.DataFrame()
        for dictionary in results:
            detections = dictionary['detections']
            for detection in detections:
                bbox = detection['bbox']
                data = {'file': dictionary['file'], 'max_detection_conf': dictionary['max_detection_conf'],
                        'category': detection['category'], 'conf': detection['conf'], 'bbox1': bbox[0],
                        'bbox2': bbox[1],
                        'bbox3': bbox[2], 'bbox4': bbox[3]}
                df = df.append(data, ignore_index=True)
        return df


def filterImages(dataframe):
    # Removes all images that MegaDetector gave no detection for
    animaldf = dataframe[dataframe['category'].astype(int) == 1]
    otherdf = dataframe[dataframe['category'].astype(int) != 1]
    return animaldf, otherdf


def extractImages(file_path, out_dir, fps=None, frames=None):
    cap = cv2.VideoCapture(file_path)
    filename = os.path.basename(file_path)
    filename, extension = os.path.splitext(filename)
    frames_saved = []

    if fps is None:
        frame_count = int(cap.get(cv2.CAP_PROP_FRAME_COUNT))
        frame_capture = 0
        increment = int(frame_count / frames)
        while cap.isOpened() and (len(frames_saved) < frames):
            cap.set(cv2.cv2.CAP_PROP_POS_FRAMES, frame_capture)
            ret, frame = cap.read()
            if not ret:
                break

            out_path = out_dir + filename + "-" + '{:05}'.format(random.randrange(1, 10 ** 5)) + "-" + str(
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

            out_path = out_dir + filename + "-" + '{:05}'.format(random.randrange(1, 10 ** 5)) + "-" + str(
                frame_capture) + '.jpg'
            cv2.imwrite(out_path, frame)
            frames_saved.append(out_path)
            frame_capture += fps

        cap.release()
        cv2.destroyAllWindows()

    return frames_saved


def imagesFromVideos(image_dir, out_dir, outfile=None, format="jpg", fps=None, frames=None, parallel=False,
                     nproc=1):
    assert os.path.isdir(image_dir), "image_dir does not exist"
    if not os.path.isdir(out_dir):
        os.makedirs(out_dir)
    files = [image_dir + x for x in os.listdir(image_dir)]
    if (fps is not None) and (frames is not None):
        print("If both fps and frames are defined fps will be used.")
    assert (fps is not None) or (frames is not None), "Either fps or frames need to be defined."
    images = []
    videos = []
    for file in files:
        filename, extension = os.path.splitext(file)
        extension = extension.lower()
        if extension == '.jpg' or extension == '.png':
            images.append(file)
        else:
            videos.append(file)

    if parallel:
        cpu_count = mp.cpu_count()
        pool = mp.Pool(cpu_count)
        videoFrames = [pool.apply(extractImages, args=(video, out_dir, fps, frames)) for video in videos]
        pool.close()
        for x in videoFrames:
            images += x
    else:
        for video in videos:
            images += (extractImages(video, out_dir=out_dir, fps=fps, frames=frames))
    return images


def symlinkClassification(data, linkdir, classes):
    table = pd.read_table(classes, sep=" ", index_col=0)
    for i in range(0, len(table.index)):
        directory = str(table['x'].values[i])
        if not os.path.isdir(linkdir + directory):
            os.makedirs(linkdir + directory)

    for i in range(0, len(data.index)):
        try:
            os.symlink(data.at[i, 'file'],
                       linkdir + data.at[i, 'class'] + "/" + os.path.basename(data.at[i, 'file']))
        except Exception as e:
            print('File already exists. Exception: {}'.format(e))
            continue


def buildFileManifest(image_dir, outfile=None):
    if outfile:
        pass
    # load file manifest
    if not os.path.isdir(image_dir):
        return "The given directory does not exist."

    files = glob.glob(os.path.join(image_dir, '**', '*.*'), recursive=True)
    images = [s for s in files if is_image(s)]

    images = pd.DataFrame(images, columns=["FilePath"])
    images["FileName"] = images["FilePath"].apply(lambda x: os.path.split(x)[1])
    images["FileModifyDate"] = images["FilePath"].apply(
        lambda x: datetime.fromtimestamp(os.path.getmtime(x)).strftime('%Y-%m-%d %H:%M:%S'))

    return images
