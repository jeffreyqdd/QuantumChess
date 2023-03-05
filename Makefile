DOCKER_IMAGE = quantum_chess:latest
CONTAINER_NAME = quantum_chess


# Build docker container
docker:
	docker buildx build . -t $(DOCKER_IMAGE)
.PHONY: test check

# Start development environment
start-container:
	docker run 																			\
		--rm																					\
		-v $(shell pwd):/home/software/quantum_chess 	\
		--detach	 																		\
		--name $(CONTAINER_NAME)											\
		$(DOCKER_IMAGE) sleep infinity

# Stop development environment
stop-container:
	docker stop $(CONTAINER_NAME)

# play the game w/o starting the dev environment
final-product:
	docker run 																													\
		--rm																															\
		-v $(shell pwd):/home/software/quantum_chess			 								\
		-it				 																												\
		$(DOCKER_IMAGE) 																									\
		/bin/bash -i -c 'clear && OCAMLRUNPARAM=b dune exec bin/main.exe'

# The following commands are used inside the docker container environment
play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

build:
	dune build

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

clean:
	dune clean 
	rm -f quantum.zip

zip:
	rm -f quantum.zip
	zip -r quantum.zip . -x@exclude.lst

check:
	@bash check.sh 

finalcheck:
	@bash check.sh final

doc:
	dune build @doc