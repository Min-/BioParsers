#!/bin/ruby

## split and merge large fastq files

## Min Zhang
## April 6, 2016

def splitAndMerge(nl, dir, file1, file2, output)
  # split
  f1 = dir + "/" + file1
  f2 = dir + "/" + file2
  tmp1 = dir + "/tmp1"
  tmp2 = dir + "/tmp2"
  tmp3 = dir + "/tmp3"
  %x{split -l #{nl} #{f1}; mv ./xa* #{tmp1}; rm ./xa*}
  %x{split -l #{nl} #{f2}; mv ./xa* #{tmp2}; rm ./xa*}
  # merge
  %x{for i in xaa xab xac xad xae xaf xag xah xai xaj; do stack exec BioParsers mergePairedEndFq #{tmp1}/$i #{tmp2}/$i > #{tmp3}/$i; done}
  %x{cat #{tmp3}/xa* > #{dir + "/" output}}
  %x{rm #{tmp1}/xa*; rm #{tmp2}/xa*; rm #{tmp3}/xa*}
end

@nl = ARGV[0]
@dir = ARGV[1]
@file1 = ARGV[2]
@file2 = ARGV[3]
@output = ARGV[4]

def main
  puts "MelvinSplitMerge 40000000 /media/martinlab/analysis/mz_P12_heart_dropseq/raw mh1.s1.l2.r1 mh1.s1.l2.r2 mh1.s1.l2" 
  splitAndMerge(@nl, @dir, @file1, @file2, @output)
end

main

