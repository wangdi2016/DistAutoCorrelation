gfortran dist-corr.f95 -o dist-corr -lm
## file1 file2 #line-in-file1 #line-in-file2 #max
#./dist-corr dyad.tsv dyad.tsv test-output 5000 5000 2000
./dist-corr dyad.tsv dyad.tsv test-output 1200
