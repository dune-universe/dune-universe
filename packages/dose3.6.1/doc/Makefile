SUBDIRS=manpages debcheck.primer apt-external-solvers.primer

all: pages
	@for subdir in $(SUBDIRS); do \
	  $(MAKE) -C $$subdir; \
	done

clean:
	@for subdir in $(SUBDIRS); do \
	  $(MAKE) -C $$subdir clean; \
	done

manpages:
	$(MAKE) -C manpages

pages:
	$(MAKE) -C rtd
