.PHONY: clean

all: documentation.pdf ../../html/documentation.html

../../html/documentation.html: documentation.typ
	typst compile --format html --features html documentation.typ ../../html/documentation.html

documentation.pdf: documentation.typ
	typst compile --features html documentation.typ documentation.pdf

clean:
	rm documentation.pdf ../../html/documentation.html