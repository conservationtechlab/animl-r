# -*- coding: utf-8 -*-
"""
Rename and resize images, create a subject set and upload new subjects to it
Created on Mon Jul 11 15:34:53 2016

@author: Mathias Tobler

Copyright (C) 2017 Mathias Tobler

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

Based on code by:
________________________________________________
Stijn Calders
Space Physics - Space Weather

Royal Belgian Institute for Space Aeronomy (BIRA-IASB)
Ringlaan 3
B-1180 Brussels
BELGIUM

phone  : +32 (0)2 373.04.19
e-mail : stijn.calders@aeronomie.be
web    : www.aeronomie.be
________________________________________________
"""

from panoptes_client import SubjectSet, Subject, Project, Panoptes
import os
import time
import sys
from datetime import datetime
from PIL import Image
from PIL import ImageFile

ImageFile.LOAD_TRUNCATED_IMAGES = True


def copy_image(infile, outpath, imageheight):
    img = Image.open(infile.FilePath)
    try:
        exif = img.info['exif']
    except KeyError:
        return None

    wpercent = (imageheight / float(img.size[1]))
    imagewidth = int((float(img.size[0]) * float(wpercent)))
    img = img.resize((imagewidth, imageheight), Image.ANTIALIAS)
    outfile = outpath + infile.FileName
    img.save(outfile, "JPEG", exif=exif)
    return outfile


def create_SubjectSet(project, subject_set_name):
    subject_set = SubjectSet()
    subject_set.links.project = project
    subject_set.display_name = subject_set_name
    subject_set.save()
    return subject_set


def connect_to_Panoptes(usr, pw):
    Panoptes.connect(username=usr, password=pw)
    print("Connected to Zooniverse")


def upload_to_Zooniverse_Simple(project_name, subject_set_name, images, outdir, imageheight=700):
    sys.stdout.flush()
    # Create a new subject set or append the subjects to an existing one
    project = Project.find(project_name)
    print("Connected to ", project)
    subject_set = SubjectSet.find(int(subject_set_name))

    print(subject_set)
    print("Uploading images...")
    print(images)
    for _, infile in images.iterrows():
        if not os.path.exists(infile.FileName):
            print(infile.FileName)
        subject = Subject()  # create new subject
        subject.links.project = project
        outfile = copy_image(infile, outdir, imageheight)
        if outfile is None:
            continue
        subject.add_location(outfile)
        subject.metadata['OrigName'] = infile.FileName
        subject.metadata['!OrigFolder'] = infile.FilePath
        subject.metadata['DateTime'] = str(infile.DateTime)
        subject.metadata['#machine_prediction'] = infile.ZooniverseCode  # map to zooniverse name
        subject.metadata['#machine_confidence'] = infile.confidence
        subject.save()
        subject_set.add(subject)


def upload_to_Zooniverse(project_name, subject_set_name, images, outdir, maxSeq=1, maxTime=0, imageheight=700):
    sys.stdout.flush()
    # Create a new subject set or append the subjects to an existing one
    project = Project.find(project_name)
    print("Connected to ", project)
    subject_set = SubjectSet.find(int(subject_set_name))

    print(subject_set)
    print("Uploading images...")

    # print(images.columns.values)

    si = 0  # set sequence counter to 0
    for _, infile in images.iterrows():
        bProc = False
        try:
            currentDateTime = time.mktime(time.strptime(infile.DateTime, "%Y-%m-%d %H:%M:%S"))
        except TypeError:
            currentDateTime = datetime.strptime(str(infile.DateTime), "%Y-%m-%d %H:%M:%S")

        print(currentDateTime)

        # add image to current sequence
        if si > 0 and si < maxSeq and (currentDateTime - dDateTime) <= maxTime and subject.metadata[
                '#machine_prediction'] == infile.ZooniverseCode:
            # print("Continue")
            outfile = copy_image(infile, outdir, imageheight)
            if outfile is None:
                continue
            subject.add_location(outfile)
            subject.metadata['OrigName'] = subject.metadata['OrigName'] + ";" + infile.FileName
            subject.metadata['!OrigFolder'] = subject.metadata['!OrigFolder'] + ";" + infile.FilePath
            subject.metadata['DateTime'] = subject.metadata['DateTime'] + ";" + infile.DateTime.strftime(
                "%Y-%m-%d %H:%M:%S")
            # subject.metadata['NewName'] = subject.metadata['NewName']+";"+ infile.NewName
            dDateTime = currentDateTime
            si = si + 1
            bProc = True
        #  print(subject.metadata)

        # sequence has ended, current image is in a new sequence
        elif 0 < si < maxSeq and ((currentDateTime - dDateTime) > maxTime or subject.metadata[
                '#machine_prediction'] != infile.ZooniverseCode):
            si = maxSeq
            bProc = False

            # need to start a new sequence OR maxSeq = 1
        if si == maxSeq:
            # print("Upload")
            subject.save()
            subject_set.add(subject)
            si = 0

        # first image within a sequence
        if si == 0 and bProc is False:
            subject = Subject()  # create new subject
            subject.links.project = project
            dDateTime = currentDateTime
            outfile = copy_image(infile, outdir, imageheight)
            if outfile is None:
                continue
            subject.add_location(outfile)
            # You can set whatever metadata you want, or none at all
            subject.metadata['OrigName'] = infile.FileName
            subject.metadata['!OrigFolder'] = infile.FilePath
            subject.metadata['DateTime'] = infile.DateTime.strftime("%Y-%m-%d %H:%M:%S")
            subject.metadata['#machine_prediction'] = infile.ZooniverseCode  # map to zooniverse name
            subject.metadata['#machine_confidence'] = infile.md_confidence
            # subject.metadata['NewName'] = infile.NewName
            si = 1
