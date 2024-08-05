# extract_timestamps.py
from PIL import Image
from exif import Image as ExifImage
import os
import pandas as pd

def get_image_timestamps(folder_path):
    timestamps = []
    for filename in os.listdir(folder_path):
        if filename.lower().endswith('.jpg'):
            filepath = os.path.join(folder_path, filename)
            with open(filepath, 'rb') as image_file:
                image = ExifImage(image_file)
                if image.has_exif and 'datetime_original' in image.list_all():
                    timestamps.append({
                        'filename': filename,
                        'timestamp': image.datetime_original
                    })
    return pd.DataFrame(timestamps)

# timestamps_df = get_image_timestamps(folder_path)
# print(timestamps_df)
