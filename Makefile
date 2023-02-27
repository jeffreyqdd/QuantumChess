DOCKER_IMAGE = quantum_chess:latest

docker:
	docker buildx build . -t $(DOCKER_IMAGE)

bash:
	docker run -v $(shell pwd):/home/software/quantum_chess -it $(DOCKER_IMAGE) /bin/bash