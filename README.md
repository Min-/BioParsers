### BioParsers
Parsing NGS data in Haskell

Min Zhang (mz1 at bcm dot edu)

## Merge Paired-End Fastq file

# problem:
 - Paired-End fastq1 fastq2
 - fastq1 and fastq2 have same read names
 - two fastq files are sorted by read names
 - merge fastq1 and fastq2 by concatenate sequences and quality
 - new read name will be original name but with fastq1 label (e.g. barcode; order number i.e. 1)

# command line:

```bash
  stack exec BioParsers mergePairedEndFq fq1 fq2 > merged.fq
```

# install:
 With stack and git
```bash
   git clone https://github.com/Min-/BioParsers.git
   cd BioParsers
   stack setup && stack build
```

# ruby script for batch processing
 - For large files (> 10G text file), 128G memory is not enough to run the program
 - Currently, use bash `split` to subset large files to small ones
 - since every fastq read takes 4 lines, devide lines by order of 4
