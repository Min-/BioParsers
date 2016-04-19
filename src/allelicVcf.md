### Find allelic variants

April 19, 2016

1. Call variants from bam file

```bash
for i in ./*addRG.bam; do java -jar /media/martinlab/tools/GenomeAnalysisTK-3.5/GenomeAnalysisTK.jar -T HaplotypeCaller -R /media/martinlab/tools/ref/genomes/mm9/mm9.fa -I $i --genotyping_mode DISCOVERY -stand_call_conf 20 -o $i.vcf; done
```

2. Get interval region of vcf

```bash
bgzip your.vcf
tabix -p vcf your.vcf.gz
tabix your.vcf.gz chr1:10,000,000-20,000,000 > output.vcf
```

