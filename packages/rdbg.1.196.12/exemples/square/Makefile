

demo:
	rdbg -sut "lv6 -esa square.lus -n square"

clean:
	rm -rf *.ec *.cm* *.log *~ .*~ *.o *rif0 *rif Data *.plot *.gp *.dro *.c rdbg-session*.ml my-rdbg-tuning.ml

# a very simple test
test:
	echo "\nfinish\nq\n" | rdbg -sut "lv6 -esa square.lus -n square"  -l 3  && \
	echo "test square ok\n"


