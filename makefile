.PHONY: all document test install

all: document test check install

document:
	Rscript -e "devtools::document()"

test:
	Rscript -e "devtools::test()"

check:
	Rscript -e "devtools::check()"

install:
	Rscript -e "devtools::install()"
