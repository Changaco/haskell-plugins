# Copyright (c) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons
# LGPL version 2.1 or later (see http://www.gnu.org/copyleft/lesser.html)


check:
	@( d=/tmp/plugins.tmp.$$$$ ; mkdir $$d ; export TMPDIR=$$d ;\
	   for i in `find testsuite -maxdepth 2 -mindepth 2 -type d | sort` ; do \
	     printf "=== testing %-50s ... " "$$i" ;\
	     ( cd $$i ; [ -f dont_test ] && echo "ignored." && exit ;\
	       ( out=`${MAKE} -sk 2>&1` ;\
	         [ $$? -ne 0 ] && echo "failed compilation:" && echo "$$out" && exit;\
	         ${MAKE} -ksi check ;\
	       ) ;\
	       ${MAKE} -sk clean ;\
	     ) ;\
	   done ; rm -rf $$d )


CLEAN_FILES += *.conf.*.old *~

EXTRA_CLEANS+=*.conf.inplace* *.conf.in *.h autom4te.cache \
              config.h config.mk config.log config.status

clean:
	cd docs && $(MAKE) clean
	runhaskell Setup.lhs clean 2> /dev/null || true
	rm -rf $(CLEAN_FILES)
	find testsuite -name '*.a' -exec rm {} \;
	find testsuite -name '*~' -exec rm {} \;
	find testsuite -name 'a.out' -exec rm {} \;
	find testsuite -name '*.hi' -exec rm {} \;
	find testsuite -name '*.o' -exec rm {} \;
	find testsuite -name '*.core' -exec rm {} \;
	find testsuite -name 'package.conf' -exec rm {} \;
	rm -f testsuite/makewith/io/TestIO.conf
	rm -f testsuite/makewith/unsafeio/Unsafe.conf
	rm -rf testsuite/plugs/plugs/plugs
	rm -rf testsuite/plugs/plugs/runplugs
	rm -rf $(EXTRA_CLEANS)

