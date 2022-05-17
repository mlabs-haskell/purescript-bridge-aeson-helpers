# Module Mapping

- `Data.Argonaut.Decode.Aeson` -> `Aeson.Decode`
- `Data.Argonaut.Decode.Decoders` (argonaut-codecs) -> `Aeson.Decode.Decoders`
- `Data.Argonaut.Encode.Aeson` -> `Aeson.Encode`
- `Data.Argonaut.Aeson` -> `Aeson.Utils`

# Future development

- Consider extracting `Aeson.Decode.Decoders` to `purescript-aeson-codecs` if it's needed outside of this project.
