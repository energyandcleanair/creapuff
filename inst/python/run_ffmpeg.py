import sys
import subprocess

# Check if the correct number of arguments are provided
if len(sys.argv) < 3:
    print("Usage: python run_ffmpeg.py output_file image_duration image_files")
    sys.exit(1)

# Extract command line arguments
output_file = sys.argv[1]
image_duration = float(sys.argv[2])
image_files = sys.argv[3:]

# Replace backslashes with forward slashes in the file paths
image_files = [file.replace('\\', '/') for file in image_files]

# Build the ffmpeg command string
ffmpeg_command = ' -y '

# Add input options for each image file
for i, image_file in enumerate(image_files):
  ffmpeg_command += f'-loop 1 -t {image_duration} -i "{image_file}" '

# Build the filter_complex string
filter_complex = ''
filter_overlay = ''

for i in range(len(image_files) - 1):
  fade_time = image_duration * i
  overlay_index = i + 1
  filter_complex += f'[{overlay_index}]fade=d={image_duration}:t=in:alpha=1,setpts=PTS-STARTPTS+{fade_time}/TB[f{i}];'

  if i == 0:
    filter_overlay += f"[0][f0]overlay[bg{overlay_index}];"
  elif i + 1 == len(image_files) - 1:
    filter_overlay += f"[bg{overlay_index - 1}][f{i}]overlay,format=yuv420p[v]"
  else:
    prev_bg = overlay_index - 1
    filter_overlay += f"[bg{prev_bg}][f{i}]overlay[bg{overlay_index}];"

filter = filter_complex+filter_overlay

# # Add the filter_complex to the ffmpeg command
ffmpeg_command += f'-filter_complex "{filter}" '

# # Add the output options
ffmpeg_command += f'-map "[v]" -movflags +faststart "{output_file}"'

print(ffmpeg_command)
# subprocess.run(ffmpeg_command, shell=True)
