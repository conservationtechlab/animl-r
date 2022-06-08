# modified from CameraTraps/MegaDetector
from TFDetector import TFDetector
from tqdm import tqdm
import cv2
import json
from FileManagement import load_image


def load_and_run_detector_batch(image_file_names, detector_file, checkpoint_path,
                                confidence_threshold, checkpoint_frequency, results):
    already_processed = set([i['file'] for i in results])

    # load the detector
    tf_detector = TFDetector(detector_file)

    count = 0  # does not count those already processed
    for im_file in tqdm(image_file_names):
        if im_file in already_processed:
            continue  # will not add additional entries not in the starter checkpoint
        if im_file.lower().endswith(".mp4"):
            try:
                _, image = cv2.VideoCapture(im_file).read()  # get first frame
            except Exception as e:
                print('Video {} cannot be loaded. Exception: {}'.format(im_file, e))
                result = {
                    'file': im_file,
                    'failure': TFDetector.FAILURE_IMAGE_OPEN
                }
                results.append(result)
                continue
        else:
            try:
                image = load_image(im_file)
            except Exception as e:
                print('Image {} cannot be loaded. Exception: {}'.format(im_file, e))
                result = {
                    'file': im_file,
                    'failure': TFDetector.FAILURE_IMAGE_OPEN
                }
                results.append(result)
                continue

        try:
            result = tf_detector.generate_detections_one_image(image, im_file, detection_threshold=confidence_threshold)
            results.append(result)
            count += 1

        except Exception as e:
            print('An error occurred while running the detector on image {}. Exception: {}'.format(im_file, e))
            continue

        # checkpoint
        if checkpoint_frequency != -1 and count % checkpoint_frequency == 0:
            print('Writing a new checkpoint after having processed {} images since last restart'.format(count))
            with open(checkpoint_path, 'w') as f:
                json.dump({'images': results}, f)

    return results  # actually modified in place
