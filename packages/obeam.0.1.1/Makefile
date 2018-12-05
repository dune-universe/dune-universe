.PHONY: build
build:
	dune build

.PHONY: build-beams
build-beams:
	make -C test compile

.PHONY: promote
promote:
	dune promote

.PHONY: test-only
test-only:
	dune runtest test

.PHONY: test
test: build-beams test-only

.PHONY: install
install:
	dune build @install

.PHONY: clean
clean:
	make -C test clean
	dune clean

.PHONY: docker-publish
docker-publish: docker-publish-ocaml docker-publish-erlang
	# NOTE: Execute "docker login" before "make docker-publish"

.PHONY: docker-publish-ocaml
docker-publish-ocaml:
	docker build -t yutopp/obeam-dev-ocaml:latest .circleci/images/ocaml
	docker push yutopp/obeam-dev-ocaml:latest

.PHONY: docker-publish-erlang
docker-publish-erlang: docker-publish-erlang-otp18 \
	docker-publish-erlang-otp19 \
	docker-publish-erlang-otp20 \
	docker-publish-erlang-otp21

.PHONY: docker-publish-erlang-otp18
docker-publish-erlang-otp18:
	docker build -t yutopp/obeam-dev-erlang:otp-18 .circleci/images/erlang/ \
		-f .circleci/images/erlang/Dockerfile.otp-18
	docker push yutopp/obeam-dev-erlang:otp-18

.PHONY: docker-publish-erlang-otp19
docker-publish-erlang-otp19:
	docker build -t yutopp/obeam-dev-erlang:otp-19 .circleci/images/erlang/ \
		-f .circleci/images/erlang/Dockerfile.otp-19
	docker push yutopp/obeam-dev-erlang:otp-19

.PHONY: docker-publish-erlang-otp20
docker-publish-erlang-otp20:
	docker build -t yutopp/obeam-dev-erlang:otp-20 .circleci/images/erlang/ \
		-f .circleci/images/erlang/Dockerfile.otp-20
	docker push yutopp/obeam-dev-erlang:otp-20

.PHONY: docker-publish-erlang-otp21
docker-publish-erlang-otp21:
	docker build -t yutopp/obeam-dev-erlang:otp-21 .circleci/images/erlang/ \
		-f .circleci/images/erlang/Dockerfile.otp-21
	docker push yutopp/obeam-dev-erlang:otp-21
