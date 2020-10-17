# pcx
Read pcx bitmap image files in R.

## About

The PCX (Picture Exchange) format is an old bitmap image format that is compressed using runlength encoding and typically indexed, i.e. used with a fixed palette. It was used together with the WAL format for parts of the assets (skybox textures, model skins) in the early Quake series games (id tech1 and tech2 engines). See the Wikipedia article for more details and a spec.


## Package API

Not ready yet, this is WIP.

    # read array with dim = width x height x channels:
    pcx = read.pcx(filepath);


