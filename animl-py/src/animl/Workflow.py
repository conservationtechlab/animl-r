import argparse
import sys
import json
import os
import pandas as pd
from datetime import datetime
from ImageCropGenerator import GenerateCropsFromFile
from FileManagement import imagesFromVideos, parseMD, filterImages
from DetectMD import load_and_run_detector_batch
from tensorflow import keras

#MegaDetector_file = "megadetector_v4.1.pb"
MegaDetector_file = '/home/edgar/mnt/machinelearning/megaDetector/megadetector_v4.1.pb'
#Classification_file = "EfficientNetB5_456_Unfrozen_01_0.58_0.82.h5"
Classification_file = "/home/edgar/mnt/machinelearning/Models/Southwest/EfficientNetB5_456_Unfrozen_01_0.58_0.82.h5"
#classes = "classes.txt"
classes = '/home/edgar/mnt/machinelearning/Models/Southwest/classes.txt'
output_file = "MD_results.json"


def main():
    parser = argparse.ArgumentParser(description='Workflow for running animl')

    parser.add_argument(
        'image_dir',
        type=str,
        help='A directory of images and/or videos to be processed')

    parser.add_argument(
        'output_dir',
        type=str,
        help='Output directory including site name and date')

    parser.add_argument(
        '--threshold',
        type=float,
        default=0.9,
        help="Confidence threshold between 0 and 1.0. Default is 0.9")

    parser.add_argument(
        '--checkpoint_frequency',
        type=int,
        default=2500,  # approximately every hour
        help='Write results to a temporary file every N images; a value of -1 disables this feature')

    parser.add_argument(
        '--resume_from_checkpoint',
        help='Initiate from the specified checkpoint, which is in the same directory as the output_file specified')

    if len(sys.argv[1:]) == 0:
        parser.print_help()
        parser.exit()

    args = parser.parse_args()

    # load the checkpoint if available
    # relative file names are only output at the end; all file paths in the checkpoint are still full paths
    if args.resume_from_checkpoint:
        assert os.path.exists(args.resume_from_checkpoint), 'File at resume_from_checkpoint specified does not exist'
        with open(args.resume_from_checkpoint) as f:
            saved = json.load(f)
        assert 'images' in saved, \
            'The file saved as checkpoint does not have the correct fields; cannot be restored'
        results = saved['images']
        print('Restored {} entries from the checkpoint'.format(len(results)))
    else:
        results = []
        images = imagesFromVideos(args.image_dir, out_dir=args.output_dir, fps=None, frames=5)

    # test that we can write to the output_file's dir if checkpointing requested
    if args.checkpoint_frequency != -1:
        checkpoint_path = os.path.join(args.output_dir,
                                       'checkpoint_{}.json'.format(datetime.utcnow().strftime("%Y%m%d%H%M%S")))
        with open(checkpoint_path, 'w') as f:
            json.dump({'images': []}, f)
        print('The checkpoint file will be written to {}'.format(checkpoint_path))
    else:
        checkpoint_path = None

    # run MegaDetector and format results
    detections = load_and_run_detector_batch(images, MegaDetector_file, checkpoint_path, args.threshold,
                                             args.checkpoint_frequency,
                                             results)

    df = parseMD(detections)
    # filter out all non animal detections
    animalDataframe, otherDataframe = filterImages(df)
    # Run generator for classification model
    generator = GenerateCropsFromFile(animalDataframe)
    # Create and predict model
    model = keras.models.load_model(Classification_file)
    predictions = model.predict(generator)
    # Format Classification results
    predictionsDataframe = pd.DataFrame(predictions)
    maxDataframe = predictionsDataframe.idxmax(axis=1).to_frame(name='class')
    maxDataframe.insert(0, 'file', animalDataframe.iloc[:, 0].to_numpy())
    # Read Classification Txt file
    table = pd.read_table(classes, sep=" ", index_col=0)
    for i in range(0, len(maxDataframe.index)):
        maxDataframe.at[i, 'class'] = table['x'].values[int(maxDataframe.at[i, 'class'])]

    # Symlink
    for i in range(0, len(table.index)):
        directory = str(table['x'].values[i])
        if not os.path.isdir(args.output_dir + directory):
            os.makedirs(args.output_dir + directory)

    for i in range(0, len(maxDataframe.index)):
        os.symlink(maxDataframe.at[i, 'file'],
                   args.output_dir + maxDataframe.at[i, 'class'] + "/" + os.path.basename(maxDataframe.at[i, 'file']))



if __name__ == '__main__':
    main()
