all:
	rm -rf guide/ master/
	git clone --depth 1 https://github.com/ninenines/erlang.mk master
	cd master/ && make docs
	mkdir guide/
	for f in master/doc/html/*.html; do \
		cp templates/guide_header.tpl guide/$${f##*/}; \
		perl -e '$$/ = undef; $$file = <>; $$file =~ s/.*<body>(.*)<\/body>.*/\1/sg; print $$file;' $$f >> guide/$${f##*/}; \
		cat templates/guide_footer.tpl >> guide/$${f##*/}; \
	done
	rm -rf master/
	git add guide/ && git commit -m "Update user guide"
