INSTALL = WuV_notes.pdf# WuV_slides.pdf
TARGET = $(INSTALL) 
GITDIR = ~/vc/git/gl.kwarc.info/kwarc/teaching/teaching/WuV
TEX = $(shell ls *.tex)
PDFLATEX = pdflatex -interaction batchmode
all: $(TARGET)

clean:
	rm -f *.aux *.toc *.synctex.gz *.log *.thm *.idx *.out *.snm *.nav
	*.bbl *.blg *.ilg *-blx.bib comment.cut *.pdflog *.vrb
	rm -Rf auto

distclean: clean
	rm -f *.data *.pdf all.tex *.sms *.ind *.ind 

$(TARGET): %.pdf: %.tex
	$(PDFLATEX) $(basename $<)
	makeindex $(basename $<) || true
	@if (grep 'Please (re)run BibTeX' $(patsubst %.dtx, %.log, $(firstword $<)) > /dev/null);\
	    then bibtex $(patsubst %.tex, %, $(firstword $<));\
	                $(PDFLATEX) $(basename $<);\
	                $(PDFLATEX) $(basename $<);fi

install: $(INSTALL)
	mkdir -p $(GITDIR)
	cd $(GITDIR); git pull; cd - 
	cp $(INSTALL) $(GITDIR)
	cd $(GITDIR); git add --force $(INSTALL);git commit -m're-generated'; git push; cd -

echo:
	@echo $(TARGET)

