# Installation of Quantum Chess Stack
This project is Dockerized and installation should be painless. For development,
follow the development section.

## Base Setup
**Step 1: Docker** 

Get docker [here](https://docs.docker.com/get-docker/)

You must create a docker group and add yourself to that group, or else the stack 
will not work as intended.

- `sudo groupadd docker`
- `sudo usermod -aG docker $USER`
- Login and logout to revaluate groups. If that does not work, restart your computer
- run `groups` (you should see docker listed)

**Step 2: Build Docker Image**\
Go into project root and run `make docker`. 

Go get a drink or do other things after running this command.
It takes around 6 minutes for a clean build (depends on internet speed and 
computer resources).

**Step 3: Play Project**\
To execute this project run `make final-product`


## Development:
To develop using the docker container env, install the [Dev Containers](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers) 
extension.

Run `make start-container` to start the environment, then go to vs-code command
pallete and execute "Dev Containers: Attach to Running Container". You should find
the quantum chess container to be up and running. 

The necessary dependencies are installed for **Microsoft Live-Share**.
