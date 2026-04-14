INARA_IMAGE ?= openjournals/inara
PAPER ?= paper.md

.PHONY: all pdf draft publish clean

all: pdf

pdf:
	docker run --rm -v "$$(pwd)":/data $(INARA_IMAGE) -o pdf $(PAPER)

draft:
	docker run --rm -v "$$(pwd)":/data $(INARA_IMAGE) $(PAPER)

publish:
	docker run --rm -v "$$(pwd)":/data $(INARA_IMAGE) -p -o pdf $(PAPER)

clean:
	rm -f paper.pdf paper.xml paper.jats
