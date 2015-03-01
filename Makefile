
PANDOC=pandoc \
     --from=markdown+lhs \
	   --to=html5 \
	   --standalone \
	   --mathjax \
	   --section-divs \
     --metadata=assets:.. \
		 --filter templates/codeblock.hs \
     --highlight-style=tango\
     --template=templates/page.template

####################################################################

all: 
	$(PANDOC) lhs/foo.lhs -o dist/foo.html

