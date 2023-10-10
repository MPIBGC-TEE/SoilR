#/bin/bash
for f in *.R
do 
	fn=${f}.new
	sed -n '1,2'p  ${sT}/runit.2MCSimulatorTest.R >${fn} 
	cat $f>>${fn}
	mv ${fn} $f
done
