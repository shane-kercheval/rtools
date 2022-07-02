####
# DOCKER
####
docker_compose:
	# docker build -t data-science-template .
	docker compose -f docker-compose.yml up --build

docker_run: zsh

zsh:
	docker exec -it rtools-bash-1 /bin/zsh

r_build:
	R -e "library(devtools); devtools::build('.')"

r_tests:
	R -e "library(devtools); devtools::test()"

tests: r_build r_tests

.PHONY: tests
