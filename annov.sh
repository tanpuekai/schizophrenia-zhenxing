#!/bin/bash

echo zhexinh
input=test.input

/software/annovar/annotate_variation.pl -geneanno -buildver hg19 $input /software/annovar/humandb/

