compile:
	wrath

build: compile
	cp .wrath/dist/* out/WEB-INF/lib/
	cp .wrath/lib/* out/WEB-INF/lib/

run: build
	MAVEN_TOKEN=$(shell sh -c "cat out/WEB-INF/appengine-web.xml | grep MAVEN_TOKEN | cut -d'\"' -f4") \
	GITHUB_TOKEN=$(shell sh -c "cat out/WEB-INF/appengine-web.xml | grep GITHUB_TOKEN | cut -d'\"' -f4") \
	java_dev_appserver.sh out

deploy: build
	gcloud app deploy --project=propensive-soundness out/

.PHONY: compile build run deploy
