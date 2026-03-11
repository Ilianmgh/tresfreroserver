.PHONY: clean

all: documentation.pdf ../../html/documentation.html help_message.txt

../../html/documentation.html: documentation.typ help_message.txt
	typst compile --format html --features html documentation.typ ../../html/documentation.html

documentation.pdf: documentation.typ help_message.txt
	typst compile --features html documentation.typ documentation.pdf

help_message.txt: ../help_message.txt
	cp ../help_message.txt ./help_message.txt

../help_message.txt:

clean:
	rm documentation.pdf ../../html/documentation.html