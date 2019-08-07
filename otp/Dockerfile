FROM debian:oldstable

WORKDIR /otp

COPY . /otp

RUN apt-get update && apt-get install -y \
  wget \
  openjdk-8-jre

RUN ./build.sh

CMD ["./run.sh"]
