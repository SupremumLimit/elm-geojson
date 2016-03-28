module GeoJson.Decoder
  ( decoder
  ) where

import GeoJson exposing (..)
import Json.Decode as JD exposing (customDecoder, Decoder, (:=), andThen)
import String

position : Decoder Position
position =
  JD.tuple2 Position JD.float JD.float


isGeometry : String -> Bool
isGeometry str =
  List.any ((==) (String.toLower str)) [ "point", "linestring", "polygon" ]


geometry : Decoder Geometry
geometry =
  ("type" := JD.string) `andThen` geometryInfo


geometryInfo : String -> Decoder Geometry
geometryInfo geometryType =
  case (String.toLower geometryType) of
    "point" ->
      JD.object1 (\pos -> Point { coordinates = pos }) ("coordinates" := position)
    "linestring" ->
      JD.object1 (\pos -> LineString { coordinates = pos }) ("coordinates" := JD.list position)
    "polygon" ->
      JD.object1 (\pos -> Polygon { coordinates = pos }) ("coordinates" := JD.list (JD.list position))
    otherwise ->
      JD.fail ("Unknown geometry type '" ++ geometryType ++ "'")


isFeature : String -> Bool
isFeature str =
  String.toLower str == "feature"


feature : Decoder Feature
feature =
  JD.object3 Feature
    ("geometry" := geometry)
    ("properties" := JD.value)
    (JD.maybe <| "id" := JD.string)


isFeatureCollection : String -> Bool
isFeatureCollection str =
  String.toLower str == "featurecollection"


featureCollection : Decoder FeatureCollection
featureCollection =
  JD.object1 FeatureCollection
    ("features" := JD.list feature)


{-| A JSON decoder to turn GeoJSON into our Elm representation
-}
decoder : Decoder GeoJson
decoder =
  ("type" := JD.string) `andThen` \geoType ->
    if isGeometry geoType then
      JD.object1 GeoJsonGeometry geometry
    else if isFeature geoType then
      JD.object1 GeoJsonFeature feature
    else if isFeatureCollection geoType then
      JD.object1 GeoJsonFeatureCollection featureCollection
    else
      JD.fail "Could not match GeoJSON type"
