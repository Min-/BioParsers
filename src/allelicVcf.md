### Find allelic variants

April 19, 2016

1. Call variants from bam file

```bash
for i in ./*addRG.bam; do java -jar /media/martinlab/tools/GenomeAnalysisTK-3.5/GenomeAnalysisTK.jar -T HaplotypeCaller -R /media/martinlab/tools/ref/genomes/mm9/mm9.fa -I $i --genotyping_mode DISCOVERY -stand_call_conf 20 -o $i.vcf; done
```

2. Get interval region of vcf

single region

```bash
bgzip your.vcf
tabix -p vcf your.vcf.gz
tabix your.vcf.gz chr1:10,000,000-20,000,000 > output.vcf
```

for multiple regions in bed file

Program: tabix (TAB-delimited file InderXer)
Version: 0.2.5 (r1005)

```bash
tabix -fB my.vcf.gz reg.bed > output.vcf
```
or

Version: 1.2.1

```bash
tabix -R reg.bed my.vcf.gz > output.vcf
```

in case the bed file needs to be sorted with bedTools

```bash
sortBed -i in.bed > out.bed
```

