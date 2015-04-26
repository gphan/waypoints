# Geocache Waypoints Optimizer

Given a GPX file with Geocache locations (see 2015.gpx for an example), this clojure application
will find an arbitrary path (using about 10 arbitrary nodes) built using a depth-first TPS solution.
Then it will apply hill climbing optimization to improve on the path until it cannot improve on it anymore.

## Usage

```
# Install leinigen
brew install leinigen

# Run it in the root
lein run

# Open the path-*.kml file in Google Earth
# Also load up the 2015.gpx file
```

## License

Copyright © 2015 Giang Phan

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
