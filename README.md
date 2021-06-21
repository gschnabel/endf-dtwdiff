## dtwdiff - ENDF file comparison utility

This program displays a comparison of two ENDF files.
It takes into account the tabular structure of the
ENDF format and is able to detect insertions and deletions
of numbers. It also takes into account approximate matches
for the identification of matching elements.

__Note__: It has not been thoroughly tested yet.

### Usage

To create the executable, run
```
gfortran -O3 -o dtwdiff dtwdiff.f90
```

Then, to compare ENDF files, execute
```
./dtwdiff <file1> <file2> | less -R
```
It outputs the contents of the files side-by-side.
Piping to `less -R` makes the output scrollable.
The left side shows the content of the first file.
Fields only present in the first file are colored red. 
The right side shows the content of the second file.
Fields only present in the second file are colored green.
Exact matches (equal strings) of corresponding fields are shown in black.
Approximate matches of numbers within a relative difference
of about 3% are colored blue.
Corresponding line numbers of the original files are also printed.
The fields are printed at the same columnar position as in the original file.  

![image](https://github.com/gschnabel/endf-dtwdiff/raw/main/screenshot.png)

In order to show only modifications, execute
```
./dtwdiff <file1> <file2> | grep -C5 '^!'
```

An html-file visualizing the differences can be created by
```
./dtwdiff <file1> <file2> | ansi2html --white > differences.html
```

### Technical details

Each MF/MT section present in both files is treated in isolation.
For each section, dynamic time warping ([DTW]) is used to identify matching elements
and to align the numbers of the MF/MT sections of the ENDF files.
Due to computational cost, the DTW is applied on chunks in each section if the section
is too large. Too large sections exceeding hundred thousand fields in total are not analyzed.

[DTW]: https://en.wikipedia.org/wiki/Dynamic_time_warping#Implementation
