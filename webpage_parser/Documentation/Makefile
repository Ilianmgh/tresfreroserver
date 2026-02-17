.PHONY: clean

all: documentation.pdf ../../html/documentation.html

../../html/documentation.html: documentation.typ
	typst compile -f html --features html documentation.typ ../../html/documentation.html

documentation.pdf: documentation.typ
	typst compile documentation.typ documentation.pdf

clean:
	rm documentation.pdf ../../html/documentation.html