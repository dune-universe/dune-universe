GNUPLOTRIF=gnuplot-rif

pdf:
	$(GNUPLOTRIF) ControleurPorte.rif -pdf
	$(GNUPLOTRIF) heater_float.rif -pdf

ps:
	$(GNUPLOTRIF) ControleurPorte.rif -ps
	$(GNUPLOTRIF) heater_float.rif -ps

cps:
	$(GNUPLOTRIF) ControleurPorte.rif -cps
	$(GNUPLOTRIF) heater_float.rif -cps


eps:
	$(GNUPLOTRIF) ControleurPorte.rif -eps
	$(GNUPLOTRIF) heater_float.rif -eps

jpg:
	$(GNUPLOTRIF) ControleurPorte.rif -jpg
	$(GNUPLOTRIF) heater_float.rif -jpg

tex:
	$(GNUPLOTRIF) ControleurPorte.rif -latex
	$(GNUPLOTRIF) heater_float.rif -latex



test-batch: pdf ps cps eps jpg tex
	ls
	sleep 1
	make clean
	echo " ==> Test went fine!"

test: pdf ps cps eps jpg tex
	sleep 1
	xpdf ControleurPorte.pdf
	xpdf heater_float.pdf
	geeqie
	make clean

clean:
	rm -f *.log *.p* *.gp *.tex *.eps *.jpg
