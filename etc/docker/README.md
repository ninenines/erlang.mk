# Create Docker image to run erlang.mk tests
* clone erlang.mk repository
```
git clone https://github.com/ninenines/erlang.mk.git
cd erlang.mk
```
* in git working dir
```
docker build . -f docker/Dockerfile
```