import glob
import subprocess
import os
import shutil
import sys

# Check if the correct number of arguments are provided
if len(sys.argv) < 5:
    print("Usage: python run_ffmpeg.py output_file image_duration batch_size image_file_paths")
    sys.exit(1)


final_output_file = sys.argv[1]
image_duration = float(sys.argv[2])
batch_size = int(sys.argv[3]) # Batch size (number of images per video)
image_file_paths = sys.argv[4]

with open(image_file_paths, "r") as file:
    paths = file.read().splitlines()
image_files = paths

# create output folder for batched videos
output_folder = 'output_videos'
# create folder for filter_complex rules
filter_folder = 'filter_complex_files'

os.makedirs(output_folder, exist_ok=True)
os.makedirs(filter_folder, exist_ok=True)

# Get a list of image files in the directory
# image_files = glob.glob(directory)

# Replace backslashes with forward slashes in the file paths
image_files = [file.replace('\\', '/') for file in image_files]

print("Images found:", len(image_files))
# Determine the number of batches
num_batches = (len(image_files) + batch_size - 1) // batch_size

# Initialize a list to store the video file names
video_files = []

# Process images in batches
for i in range(num_batches):
    # Get the current batch of image files
    batch_files = image_files[i * batch_size: (i + 1) * batch_size]

    # Build the ffmpeg command string for the current batch
    ffmpeg_command = 'ffmpeg -y '

    # Add input options for each image file in the batch
    for image_file in batch_files:
        ffmpeg_command += f"-loop 1 -t {image_duration} -i {image_file} "

    # Build the filter_complex string
    filter_complex = ''
    filter_overlay = ''
    for j in range(len(batch_files) - 1):
        fade_time = (image_duration - 0.1) * j
        overlay_index = j + 1
        filter_complex += f'[{overlay_index}]fade=d={image_duration}:t=in:alpha=1,setpts=PTS-STARTPTS+{fade_time}/TB[f{j}];'

        if j == 0:
            if j + 1 == len(batch_files) - 1:
                filter_overlay += f"[0][f0]overlay,format=yuv420p[v]"
            else:
                filter_overlay += f"[0][f0]overlay[bg{overlay_index}];"
        elif j + 1 == len(batch_files) - 1:
            filter_overlay += f"[bg{overlay_index - 1}][f{j}]overlay,format=yuv420p[v]"
        else:
            prev_bg = overlay_index - 1
            filter_overlay += f"[bg{prev_bg}][f{j}]overlay[bg{overlay_index}];"

    filter = filter_complex + filter_overlay

    # create a file for the current batch containing the filter rules
    filter_file_path = os.path.join(filter_folder, f'filter_complex_{i}.txt')
    with open(filter_file_path, 'w') as filter_file:
        filter_file.write(filter_complex + filter_overlay)
    # Add the filter_complex and output options to the ffmpeg command for the current batch,
    # since the filter_complex string can get very large we append it to a file and use the file path instead
    output_file = os.path.join(output_folder, f'output_{i}.mp4')
    ffmpeg_command += f'-filter_complex_script {filter_file_path} -map "[v]" -movflags +faststart -r 5 -c:v libx265 {output_file}'

    # Print the batch information
    print(f"Processing Batch {i + 1}/{num_batches} - Images: {len(batch_files)}")

    # Print and run the ffmpeg command for the current batch
    print(ffmpeg_command)
    subprocess.run(ffmpeg_command, shell=True, check=True)

    # Add the output file name to the list
    video_files.append(output_file)

# Concatenate the videos
concat_command = f'ffmpeg -y -f concat -safe 0 -i concat_list.txt -c copy {final_output_file}'

# Generate the concat list file
with open('concat_list.txt', 'w') as f:
    for video_file in video_files:
        f.write(f"file '{video_file}'\n")

# Print and run the concatenate command
subprocess.run(concat_command, shell=True, check=True)

shutil.rmtree(output_folder)
shutil.rmtree(filter_folder)
