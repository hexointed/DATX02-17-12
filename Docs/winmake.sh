for f in figure/*.svg; 
do 
	"c:\Program\Doc\Inkscape\inkscape.com" -D -z --file=$f  --export-pdf=${f%%.*}.pdf
	#echo ${f%%.*}.pdf
done

bibtex main