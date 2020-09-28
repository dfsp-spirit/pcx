# pcx
Read pcx bitmap image files in R.

## About

The PCX (Picture Exchange) format is an old bitmap image format that is compressed using runlength encoding and indexed. It was used for assets (textures, model skins) in the early Quake series games (id tech1 and tech2 engines). See the Wikipedia article for more details and a spec.

## Package API

    # read array with dim = width x height x channels:
    pcx = read.pcx(filepath);


