# frogger
An OTP application

## Build

```
# Install deps
sudo apt-get install libsdl2-dev libsdl2-ttf-dev libsdl2-image-dev

# Make sure old build are cleaned out
rm -rf _build
rebar3 clean

# Build
rebar3 compile

# Run using the rebar3_run plugin
rebar3 run
```

## Other

```
# Check dependencies
rebar3 deps

```


# Assets
https://github.com/HammadSiddiqui/frogger-sfml/tree/master/assets
https://www.spriters-resource.com/arcade/frogger/sheet/11067/

https://excamera.com/sphinx/gameduino/tutorials/frogger1.html
https://github.com/stamina/frogger-gameduino/blob/master/frogger.c
https://github.com/HammadSiddiqui/frogger-sfml

# Debug esdl2

```
# Edit C file, like add:
# printf("FAIL\n");
make -C ./_build/default/lib/esdl2/
rebar shell
```

