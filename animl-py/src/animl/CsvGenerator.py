import argparse
import sys
import os
import csv
import time

def main():
    parser = argparse.ArgumentParser(description='Generates CSV file containing raw image data')

    parser.add_argument(
        '-image_dir',
        type=str,
        help='A directory of images and/or videos to be processed')

    parser.add_argument(
        '-output_dir',
        type=str,
        help='Output directory including site name and date')

    if len(sys.argv[1:]) == 0:
        parser.print_help()
        parser.exit()

    args = parser.parse_args()

    image_dir = args.image_dir
    output_dir = args.output_dir

    files = list_files(image_dir, ".jpg")

    output_file_name = "{dir}/animal_{time}.csv".format(dir=output_dir, time=current_milli_time())
    output_file = open(output_file_name, 'w')
    writer = csv.writer(output_file)
    header = ["species", "path_to_file", "file_name"]
    writer.writerow(header)

    for file_path in files:
        file_parts = file_path.split("/")
        file_name = file_parts[-1]
        name_parts = file_name.split("_")
        species_name = name_parts[-1].split(".jpg")[0]

        species_row = [species_name, file_path, file_name]
        writer.writerow(species_row)

    output_file.close()
    print("Finished writing to: " + output_file_name)


def list_files(filepath, filetype):
   paths = []
   for root, dirs, files in os.walk(filepath):
      for file in files:
         if file.lower().endswith(filetype.lower()):
            paths.append(os.path.join(root, file))
   return(paths)

def current_milli_time():
    return round(time.time() * 1000)

if __name__ == '__main__':
    main()
