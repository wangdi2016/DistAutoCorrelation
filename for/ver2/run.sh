gfortran dist-corr.f95 -o dist-corr
## file1 file2 #line-in-file1 #line-in-file2 #max
./dist-corr dyad.tsv dyad.tsv test-output 2666 2666 1200
./dist-corr dyad.tsv dyad.tsv test-output2 3000 3000 1200
