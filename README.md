# geojson-types

Provides data types, lens operators and (de)serialization
of GeoJSON data to/from JSON and BSON using aeson and bson.

This library uses a the lens library a lot. It provides
'Iso' / 'Prism' to convert from and to GeoJSON objects.

The library also provides type classes for working polymorphic
over user defined data types.
