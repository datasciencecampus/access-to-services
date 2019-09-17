FROM debian:oldstable

WORKDIR /otp

COPY . /otp

RUN apt-get update && apt-get install -y \
  wget \
  openjdk-8-jre \
  libbz2-dev \
  zlib1g-dev \
  bzip2

RUN ./build.sh

CMD ["./run.sh"]