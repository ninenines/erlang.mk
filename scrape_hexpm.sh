#!/usr/bin/sh

# This script will scrape packages from hex.pm that are believed to be
# Erlang packages. We do this by first walking through all packages and
# then getting the most recent release of each package. If the build tools
# listed contain "rebar3" we keep them and write the package
# name and version to a file.
#
# This script should only be run occasionally to refresh the file
# containing the list of packages.
#
# @todo Some of the projects fetched are Elixir despite indicating
#       "rebar3" or "make". We should ignore them here once identified
#       so they don't make it to the output.

NUM=1

while true; do

	echo "# Packages page $NUM"

	PAGE=$(curl -s "https://hex.pm/api/packages?sort=name&page=$NUM")

	if [ "$PAGE" = "[]" ]; then exit 0; fi

	PACKAGES=$(echo $PAGE | jq -r "map({name: .name, url: .releases[0].url})")

	echo $PACKAGES | jq -r '.[] | [.name, .url] | join(" ")' | while read -r NAMEURL; do

		NAME=$(echo $NAMEURL | awk '{print $1;}')
		URL=$(echo $NAMEURL | awk '{print $2;}')

		VERSION=$(curl -s "$URL" | jq 'select(.meta.build_tools | index("rebar3")) | .version | tostring')
		VERSION=$(echo $VERSION | tr -d '"')

		if [ -n "$VERSION" ]; then
			echo "$NAME $VERSION"
		fi

	done

	NUM=$(expr $NUM + 1)

	sleep 10

done
