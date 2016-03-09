default:
	pandoc -f markdown+lhs+yaml_metadata_block --highlight-style=haddock -S -c "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css" --section-divs -c tutorial.css --toc --standalone -t html5 -o tutorial.html tutorial.lhs
	pandoc -f markdown+lhs+yaml_metadata_block --highlight-style=haddock --toc --standalone -t rst -o tutorial.rst tutorial.lhs
	pandoc -f markdown+lhs+yaml_metadata_block --highlight-style=haddock --toc --standalone -t latex -o tutorial.pdf tutorial.lhs
