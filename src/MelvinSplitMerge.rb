#!/bin/ruby

## split and merge large fastq files

## Min Zhang
## April 6, 2016

def splitAndMerge(nl, dir, file1, file2, output)
  # split
  f1 = dir + "/" + file1
  f2 = dir + "/" + file2
  %x{split -l #{nl} #{f1}}
  %x{mv xa* #{dir + "/tmp1"}; rm xa*}
  %x{split -l #{nl} #{f2}; mv xa* #{dir + "/tmp2"}; rm xa*}
  # merge
  %x{for i in xaa xab xac xad xae xaf xag; do stack exec BioParsers mergePairedEndFq #{dir + "/tmp1/"}$i #{dir + "/tmp2/"}$i > #{dir + "/tmp3/"}$i; done}
  %x{cat #{dir + "/tmp3/xa*"} > #{output}}
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

