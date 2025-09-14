# Following advice from https://fmartingr.com/blog/2024/03/12/create-an-audiobook-file-from-several-mp3-files-using-ffmpeg/
abook() {
      # Set output filename based on the current directory name
      local output_file="$(basename "$PWD").m4b"
      local temp_list="$(mktemp)"
      local metadata_file="$(mktemp)"
      local temp_mp3="$(mktemp --suffix=.mp3)"
      local temp_mp3_with_cover="$(mktemp --suffix=.mp3)"
      local temp_m4a="$(mktemp --suffix=.m4a)"
      local temp_m4b="$(mktemp --suffix=.m4b)"

      # Cleanup temporary files when the script exits
      cleanup() {
          rm -f "$temp_list" "$metadata_file" "$temp_mp3" "$temp_m4b" "$temp_mp3_with_cover"
      }
      trap cleanup EXIT

      # Display help message
      if [[ "$1" == "-h" || "$1" == "--help" ]]; then
          echo "Usage: abook"
          echo "Merges all MP3 files and cover.jpg in the current directory into a single M4B\n"
          echo "named after the folder.  Use the cover.jpg in the current folder.\n"
          echo "There are idiomatic file naming conventions to consider."
          return 0
      fi

      # Remove existing output files if they exist
      rm -f "$output_file" "$temp_list" "$metadata_file" "$temp_mp3" "$temp_m4b" "$temp_mp3_with_cover"

      # Create a list of MP3 files for concatenation (excluding temp.mp3)
      create_mp3_list() {
          rm -f "$temp_list"
          find . -maxdepth 1 -type f -name "*.mp3" ! -name "$(basename "$temp_mp3")" | sort | while read -r file; do
              echo "file '$(realpath "$file")'" >> "$temp_list"
          done
      }

      # Merge MP3 files using ffmpeg
      merge_mp3() {
          ffmpeg -f concat -safe 0 -i "$temp_list" -c copy "$temp_mp3"
          [[ ! -f "$temp_mp3" ]] && { echo "Error: Failed to create temp.mp3"; return 1; }
      }

      add_cover_to_mp3() {
          ffmpeg -i "$temp_mp3" -i cover.jpg -c copy -map 0 -map 1 "$temp_mp3_with_cover"
          [[ ! -f "$temp_mp3_with_cover" ]] && { echo "Error: Failed to add cover.jpg"; return 1; }
      }

      convert_to_m4a() {
          ffmpeg -i "$temp_mp3_with_cover" -c:v copy "$temp_m4a"
      }

      # Generate chapter metadata
      generate_metadata() {
          echo ";FFMETADATA1" > "$metadata_file"
          echo "title=$(basename "$PWD")" >> "$metadata_file"
          echo "artist=<AUTHOR>" >> "$metadata_file"
          echo "composer=<NARRATOR>" >> "$metadata_file"
          echo "album=$(basename "$PWD")" >> "$metadata_file"

          local index=1
          local start_time=0

          find . -maxdepth 1 -type f -name "*.mp3" ! -name "$(basename "$temp_mp3")" | sort | while read -r file; do
              local duration=$(ffmpeg -i "$file" 2>&1 | grep "Duration" | awk '{print $2}' | tr -d ,)

              # Convert time to milliseconds
              local seconds=$(echo "$duration" | awk -F: '{ print ($1 * 3600) + ($2 * 60) + $3 }')
              local start_ms=$(echo "$start_time * 1000" | bc)
              local end_ms=$(echo "($start_time + $seconds) * 1000" | bc)
              local chapter=$(echo "$file" | cut -d"-" -f2 | sed -e 's/^[[:space:]]*//' | sed -e 's/\.mp3//')

              echo "" >> "$metadata_file"
              echo "[CHAPTER]" >> "$metadata_file"
              echo "TIMEBASE=1/1000" >> "$metadata_file"
              echo "START=$start_ms" >> "$metadata_file"
              echo "END=$end_ms" >> "$metadata_file"
              echo "title=$chapter" >> "$metadata_file"

              start_time=$(echo "$start_time + $seconds" | bc)
              index=$((index + 1))
          done
      }

      # Add metadata to the final M4B file
      add_metadata() {
          ffmpeg -i "$temp_m4a" -i "$metadata_file" -map 0 -map_metadata 1 -c copy "$output_file"
          echo "M4B file created: $output_file"
      }

      # Execute functions
      echo "=~=~= Creating MP3 List"
      create_mp3_list
      echo "=~=~= Generating Metadata"
      generate_metadata
      echo "=~=~= Merging MP3"
      merge_mp3
      echo "=~=~= Adding Cover Image to MP3"
      add_cover_to_mp3
      echo "=~=~= Convert to m4a"
      convert_to_m4a
      echo "=~=~= Adding Metadata and Finalize"
      add_metadata
  }
