##########################################################################
#                                                                        #
#                 ACG development toolkit                                #
#                                                                        #
#                  Copyright 2008 INRIA                                  #
#                                                                        #
#  More information on "http://acg.gforge.inria.fr/"                     #
#  License: CeCILL, see the LICENSE file or "http://www.cecill.info"     #
#  Authors: see the AUTHORS file                                         #
#                                                                        #
#                                                                        #
#                                                                        #
#                                                                        #
#  $Rev::                              $:  Revision of last commit       #
#  $Author::                           $:  Author of last commit         #
#  $Date::                             $:  Date of last commit           #
#                                                                        #
##########################################################################

.PHONY: clean install uninstall tar www release test-www-acg opam-meta opam version

VERSION_BASENAME=$(shell cat acgtk.version)
VERSION = $(VERSION_BASENAME)-$(shell date "+%Y%m%d")
RELEASE = acg-$(VERSION)

TAR_RELEASE = $(RELEASE)

ACGtk_DIR = $(shell basename `pwd`)

clean:
	jbuilder clean
	find . -name "*~" -exec rm -f {} \;

install:
	jbuilder install

uninstall:
	jbuilder uninstall

tar:
	if test -d ../$(TAR_RELEASE) ; then rm ../$(TAR_RELEASE) ; fi
	cd .. && ln -s $(ACGtk_DIR) $(TAR_RELEASE) && cd $(ACGtk_DIR)
	echo Building $(TAR_RELEASE).tar.gz
	tar --exclude="*/.git*" --exclude "*/_build" --exclude "$(TAR_RELEASE)/data" --exclude "$(TAR_RELEASE)/src/data" --exclude "$(TAR_RELEASE)/*.tar.*" --exclude "$(TAR_RELEASE)/TODO" --exclude "*~" --exclude "opam-package" -zcvf $(TAR_RELEASE).tar.gz -C ..  -h $(TAR_RELEASE) 
	if test -d ../$(TAR_RELEASE) ; then rm ../$(TAR_RELEASE) ; fi

www:
	cp acg-$(VERSION).tar.gz ~/www-acg/software/.
	scp acg-$(VERSION).tar.gz pogodall@loria.loria.fr:www-acg/software/.
	scp acg-$(VERSION).tar.gz pogodall@loria.loria.fr:www-calligramme-acg/software/.
	scp INSTALL pogodall@loria.loria.fr:www-acg/software/.
	scp INSTALL pogodall@loria.loria.fr:www-calligramme-acg/software/.
	scp README pogodall@loria.loria.fr:www-acg/software/.
	scp README pogodall@loria.loria.fr:www-calligramme-acg/software/.
	cp INSTALL ~/www-acg/software/.
	cp README ~/www-acg/software/.
	sed -i -e 's/acg-[^-]*-[0-9]*\.tar\.gz/acg-$(VERSION).tar.gz/' ~/www-acg/index.html
	scp ~/www-acg/index.html pogodall@loria.loria.fr:www-acg/.
	scp ~/www-acg/index.html pogodall@loria.loria.fr:www-calligramme-acg/.

version:src/utils/version.ml

src/utils/version.ml:acgtk.version
	sed -i -e 's/VERSION-DATE/$(VERSION)/g' $@

release : version tar www
	if svn status -q -u | grep -v "^Status against" | grep -q -v "opam-package/url" | grep -q -v "^\?" ; \
	then  \
	printf "\n\nERROR:\nPlease commit before making a release\n\n" ; \
	else \
	if svn list ^/dev/tags/release-$(VERSION) > /dev/null ; then \
	printf "\n\nERROR: the svn tag directory already exists\n" ; \
	else \
	svn cp ^/dev/trunk ^/dev/tags/release-$(VERSION) -m "Tagging the $(VERSION) release of the 'acg' project" ; \
	fi \
	fi

OPAM_TESTING_DIR=~/work/dev/opam-contrib/testing/packages
OPAM_GIT_REPO_DIR=~/work/dev/opam-contrib/opam-repository/packages
OPAM_ACG_REPO_DIR=~/www-acg/software/opam-acg
OPAM_CALLIGRAMME_ACG_REPO_DIR=~/www-calligramme-acg/software/opam-acg

opam-package/url: tar
	md5sum acg-$(VERSION).tar.gz | cut -d " " -f 1 | xargs -i printf "archive: \"http://calligramme.loria.fr/acg/software/acg-$(VERSION).tar.gz\"\nchecksum: \"%s\"\n" {} > $@

test-www-acg:
	cd $(OPAM_ACG_REPO_DIR) && opam-admin make && scp -r . pogodall@loria.loria.fr:$(OPAM_ACG_REPO_DIR) && scp -r . pogodall@loria.loria.fr:$(OPAM_CALLIGRAMME_ACG_REPO_DIR)

opam-meta:
	cp -rT opam-package $(OPAM_TESTING_DIR)/acgtk/acgtk.$(VERSION_BASENAME)
	cp -rT opam-package $(OPAM_GIT_REPO_DIR)/acgtk/acgtk.$(VERSION_BASENAME)
	cp -rT opam-package $(OPAM_ACG_REPO_DIR)/packages/acgtk/acgtk.$(VERSION_BASENAME) && cd $(OPAM_ACG_REPO_DIR) && opam-admin make && scp -r . pogodall@loria.loria.fr:$(OPAM_ACG_REPO_DIR) && scp -r . pogodall@loria.loria.fr:$(OPAM_CALLIGRAMME_ACG_REPO_DIR)


opam:opam-package/url www
	cp -rT opam-package $(OPAM_TESTING_DIR)/acgtk/acgtk.$(VERSION_BASENAME)
	cp -rT opam-package $(OPAM_GIT_REPO_DIR)/acgtk/acgtk.$(VERSION_BASENAME)
	cp -rT opam-package $(OPAM_ACG_REPO_DIR)/packages/acgtk/acgtk.$(VERSION_BASENAME) && cd $(OPAM_ACG_REPO_DIR) && opam-admin make && scp -r . pogodall@loria.loria.fr:$(OPAM_ACG_REPO_DIR) && scp -r . pogodall@loria.loria.fr:$(OPAM_CALLIGRAMME_ACG_REPO_DIR)
