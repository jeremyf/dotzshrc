# --------------------------------------------------------------------
# Recursively find pdfs from the directory given as the first argument,
# otherwise search the current directory.
# Use exiftool and qpdf (both must be installed and locatable on $PATH)
# to strip all top-level metadata from PDFs.
#
# Note - This only removes file-level metadata, not any metadata
# in embedded images, etc.
#
# Code is provided as-is, I take no responsibility for its use,
# and I make no guarantee that this code works
# or makes your PDFs "safe," whatever that means to you.
#
# You may need to enable execution of this script before using,
# eg. chmod +x clean_pdf.sh
#
# example:
# clean current directory:
# >>> ./clean_pdf.sh
#
# clean specific directory:
# >>> ./clean_pdf.sh some/other/directory
# --------------------------------------------------------------------


# Color Codes so that warnings/errors stick out
GREEN="\e[32m"
RED="\e[31m"
CLEAR="\e[0m"

# loop through all PDFs in first argument ($1),
# or use '.' (this directory) if not given
DIR="${1:-.}"

echo "Cleaning PDFs in directory $DIR"

# use find to locate files, pip to while read to get the
# whole line instead of space delimited
# Note -- this will find pdfs recursively!!
find $DIR -type f -name "*.pdf" | while read -r i
do

  # output file as original filename with suffix _clean.pdf
  TMP=${i%.*}_clean.pdf

  # remove the temporary file if it already exists
  if [ -f "$TMP" ]; then
      rm "$TMP";
  fi

  exiftool -q -q -all:all= "$i" -o "$TMP"
  qpdf --linearize --deterministic-id --replace-input "$TMP"
  echo -e $(printf "${GREEN}Processed ${RED}${i} ${CLEAR}as ${GREEN}${TMP}${CLEAR}")

done
