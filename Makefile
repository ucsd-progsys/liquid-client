
PANDOC=pandoc \
     --from=markdown+lhs \
	 --to=html5 \
	 --standalone \
	 --mathjax \
	 --section-divs \
	 --filter templates/codeblock.hs \
     --highlight-style=tango\
     --template=templates/page.template

PANDOCHTML=pandoc \
     --from=markdown+lhs \
	 --to=html5 \
     -s --mathjax \
	 --standalone \
     --parse-raw \
	 --mathjax \
	 --toc \
	 --section-divs \
	 --filter templates/codeblock.hs \
	 --variable=root:"../"\
	 --highlight-style=tango \
	 --template=templates/page.template

####################################################################

all: 
	$(PANDOCHTML) lhs/foo.lhs -o dist/foo.html

