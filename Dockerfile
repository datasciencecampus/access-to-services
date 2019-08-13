FROM debian

WORKDIR /stuff/propeR

COPY . /stuff

RUN apt-get update && apt-get install -y \
  make \
  r-base

RUN make 

#CMD ["./run.sh"]
