all: schedule.pdf

schedule.pdf: schedule.tex index.tex
	pdflatex schedule.tex

index.tex: main.rkt
	racket -t main.rkt
