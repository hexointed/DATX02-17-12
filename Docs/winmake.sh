dot -Tsvg -ofigure/dots/GPU-schematic.svg figure/dots/GPU-schematic.dot

for f in figure/dots/*.svg; 
do 
	"c:\Program\Doc\Inkscape\inkscape.com" -D -z --file=$f  --export-pdf=${f%%.*}.pdf
	#"c:\Program\Doc\Inkscape\inkscape.com" -D -z --file=$f  --export-pdf=figure/$(basename ${f%%.*}).pdf
	#echo ${f%%.*}.pdf
	#echo $(basename ${f%%.*})
done

for f in figure/*.svg; 
do 
	"c:\Program\Doc\Inkscape\inkscape.com" -D -z --file=$f  --export-pdf=${f%%.*}.pdf
	#echo ${f%%.*}.pdf
done

bibtex main