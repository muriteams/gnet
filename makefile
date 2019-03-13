all:
	cd ../ && R CMD build --no-manual gnet/ && R CMD check --as-cran --use-valgrind --no-manual gnet_*.tar.gz
