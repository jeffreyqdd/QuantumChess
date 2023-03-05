# Installation of Quantum Chess Stack
This project is Dockerized and installation should be painless. For development,
follow the development section.

## Docker
**Step 1: Docker** \
Get docker [here](https://docs.docker.com/get-docker/)

**Step 2: Build Docker Image**\
Go into project root and run `make docker`.

**Step 3: Play Project**\
To execute this project run `make final-product`


## Development:
To develop using the docker container env, install the [Dev Containers](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers) 
extension.

Run `make start-container` to start the environment, then go to vs-code command
pallete and execute "Dev Containers: Attach to Running Container"
